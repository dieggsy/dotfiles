#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <dirent.h>
#include <string.h>
#include <dbus/dbus.h>
#include <espeak/speak_lib.h>

static int read_int(const char* filename);
static int get_joint_percent();
static char *get_icon (int adapter_online, int percent);
static void notify(char *title, char* body);
static void say(char *text);
static int build_message(DBusMessage *msg_notify, char *title, char *body);
static void check_and_abort(DBusError *error);
static int round_multiple(int strength, int multiple);

int main () {
    int percent = get_joint_percent();
    int adapter_online = read_int("/sys/class/power_supply/AC/online");

    printf("%s %d", get_icon(adapter_online, percent), percent);
    if (!adapter_online && percent <= 10) {
        notify("Battery critically low", "Consider charging");
        say("Battery critically low, consider charging");
    }
    return 0;
}

static int read_int(const char* filename) {
    int energy;
    FILE *f = fopen(filename,"r");
    fscanf(f,"%d",&energy);
    return energy;
}

static int get_joint_percent() {
    char * parent_dir = "/sys/class/power_supply/";
    char * fname_now = "energy_now";
    char * fname_full = "energy_full";

    float numerator=0;
    float denominator=0;
    DIR* dir = opendir(parent_dir);
    struct dirent *ent = readdir(dir);
    while (ent) {
        char* name = ent->d_name;
        if (strncmp(name, "BAT", 3) == 0) {
            size_t nowlen = snprintf(NULL, 0, "%s%s/%s", parent_dir, name, fname_now) + 1;
            size_t fulllen = snprintf(NULL, 0, "%s%s/%s", parent_dir, name, fname_full ) + 1;
            char * nowpath = malloc(nowlen);
            char * fullpath = malloc(fulllen);
            snprintf(nowpath, nowlen, "%s%s/%s", parent_dir, name, fname_now);
            snprintf(fullpath, fulllen, "%s%s/%s", parent_dir, name, fname_full);
            numerator += read_int(nowpath);
            denominator += read_int(fullpath);
            free(nowpath);
            free(fullpath);
        }
        ent = readdir(dir);
    }

    /* return 0; */
    return (int)roundf(numerator/denominator * 100);
}

static char * get_icon (int adapter_online, int percent) {
    char* icon;
    percent = round_multiple(percent, 33);
    if (percent == 99) {
        icon = adapter_online ? "{###}" : "[###]";
    } else if (percent == 66) {
        icon = adapter_online ? "{## }" : "[## ]";
    } else if (percent == 33) {
        icon = adapter_online ? "{#  }" : "[#  ]";
    } else {
        icon = adapter_online ? "{   }" : "[   ]";
    }
    return icon;
}

static void notify(char *title, char* body) {
	DBusMessage* msg_notify;
	DBusConnection* connection;
	DBusError error;

	dbus_error_init(&error);
	connection = dbus_bus_get(DBUS_BUS_SESSION, &error);
    check_and_abort(&error);

    msg_notify = dbus_message_new_method_call("org.freedesktop.Notifications",
                                              "/org/freedesktop/Notifications",
                                              "org.freedesktop.Notifications",
                                              "Notify");
    build_message(msg_notify, title, body);

    dbus_connection_send(connection, msg_notify, NULL);
    dbus_connection_flush(connection);

    dbus_message_unref(msg_notify);
}

static void say(char *text) {
    static int initialized = 0;

    if (!initialized) {
        espeak_Initialize(AUDIO_OUTPUT_PLAYBACK, 0, NULL, 0);

        espeak_VOICE voice = {0};
        voice.variant = 2;
        voice.gender = 2;

        espeak_SetVoiceByProperties(&voice);
        initialized = 1;
    }

    espeak_Synth(text, strlen(text) +1, 0, POS_CHARACTER, 0, espeakCHARS_AUTO,
                 NULL, NULL);
    espeak_Synchronize();
}

static int build_message(DBusMessage *msg_notify, char *title, char *body) {
    DBusMessageIter args, actions, hints;
    int replaces_id = -1;
    int timeout = 0;
    char* app_name = "";
    char* app_icon = "";

    dbus_message_iter_init_append(msg_notify, &args);
    int returnCode = dbus_message_iter_append_basic(&args, DBUS_TYPE_STRING, &app_name);
    returnCode |= dbus_message_iter_append_basic(&args, DBUS_TYPE_UINT32, &replaces_id);
    returnCode |= dbus_message_iter_append_basic(&args, DBUS_TYPE_STRING, &app_icon);
    returnCode |= dbus_message_iter_append_basic(&args, DBUS_TYPE_STRING, &title);
    returnCode |= dbus_message_iter_append_basic(&args, DBUS_TYPE_STRING, &body);

    returnCode |= dbus_message_iter_open_container(&args, DBUS_TYPE_ARRAY, DBUS_TYPE_STRING_AS_STRING, &actions);
    returnCode |= dbus_message_iter_close_container(&args, &actions);

    returnCode |= dbus_message_iter_open_container(&args,
                                                   DBUS_TYPE_ARRAY,
                                                   DBUS_DICT_ENTRY_BEGIN_CHAR_AS_STRING
                                                   DBUS_TYPE_STRING_AS_STRING
                                                   DBUS_TYPE_VARIANT_AS_STRING
                                                   DBUS_DICT_ENTRY_END_CHAR_AS_STRING,
                                                   &hints);
	returnCode |= dbus_message_iter_close_container(&args, &hints);

	returnCode |= dbus_message_iter_append_basic(&args, DBUS_TYPE_INT32, &timeout);

	return returnCode;
}

static void check_and_abort(DBusError *error) {
    if (!dbus_error_is_set(error)) return;
    puts(error->message);
    dbus_error_free(error);
    abort();
}

int round_multiple(int strength, int multiple) {
    return ((strength + multiple/2)/multiple) * multiple;
}
