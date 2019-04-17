#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <dirent.h>
#include <string.h>
#include <systemd/sd-bus.h>
#include <espeak-ng/speak_lib.h>


int read_int(const char* filename) {
    int energy;
    FILE *f = fopen(filename,"r");
    fscanf(f,"%d",&energy);
    return energy;
}

int get_joint_percent() {
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

char * get_icon (int adapter_online) {
    if (adapter_online) {
        return "";
    }
    return "";
}

int notify(char *title, char* body) {
    int r;
    sd_bus *bus=NULL;
    sd_bus_message *msg=NULL;
    sd_bus_error error = SD_BUS_ERROR_NULL;

    r = sd_bus_open_user(&bus);
    if (r < 0 ) {
        return r;
    }
    r = sd_bus_call_method(bus,
                           "org.freedesktop.Notifications",
                           "/org/freedesktop/Notifications",
                           "org.freedesktop.Notifications",
                           "Notify",
                           &error, &msg, "susssasa{sv}i",
                           NULL, 0, NULL, title, body, 0, 0, -1);

    if (r < 0) {
        return r;
    }
    return 0;
}

void say(char *text) {
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

int main () {
    int percent = get_joint_percent();
    int adapter_online = read_int("/sys/class/power_supply/AC/online");

    printf("%s %d", get_icon(adapter_online), percent);
    if (!adapter_online && percent <= 10) {
        notify("Battery critically low", "Consider charging");
        say("Battery critically low, consider charging");
    }
    return 0;
}
