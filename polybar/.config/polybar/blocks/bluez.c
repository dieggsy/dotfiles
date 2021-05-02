#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dbus/dbus.h>

static void check_and_abort(DBusError *error);

int main() {
    DBusError error;
    dbus_error_init(&error);

    DBusConnection *connection = dbus_bus_get(DBUS_BUS_SYSTEM, &error);
    check_and_abort(&error);
    DBusMessage *msg_query = dbus_message_new_method_call("org.bluez",
                                                          "/",
                                                          "org.freedesktop.DBus.ObjectManager",
                                                          "GetManagedObjects");
    DBusMessage *msg_reply = dbus_connection_send_with_reply_and_block(connection,
                                                                       msg_query,
                                                                       -1,
                                                                       &error);
    dbus_message_unref(msg_query);
    check_and_abort(&error);
    // Enter dict
    DBusMessageIter pathdict;
    dbus_message_iter_init(msg_reply, &pathdict);
    // Get length of dict
    int len = dbus_message_iter_get_element_count(&pathdict);
    // Go to first entry
    DBusMessageIter pathdict_entry;
    dbus_message_iter_recurse(&pathdict, &pathdict_entry);
    DBusMessageIter pathdict_kv;
    char *path, *iface, *prop;
    if (len == 1) {
        // Bluetooth is off, probably
        return 0;
    }
    while (len--) {
        // pathdict_kv points to the key in the first entry, which is a path.
        dbus_message_iter_recurse(&pathdict_entry, &pathdict_kv);
        dbus_message_iter_get_basic(&pathdict_kv, &path);
        /* printf("%s\n",path); */
        // pathdict_kv now points to the value in the first entry, which is a
        // dict where keys are interfaces.
        dbus_message_iter_next(&pathdict_kv);
        int len = dbus_message_iter_get_element_count(&pathdict_kv);
        DBusMessageIter ifacedict_entry, ifacedict_kv;
        dbus_message_iter_recurse(&pathdict_kv, &ifacedict_entry);
        while (len-- > 0) {
            dbus_message_iter_recurse(&ifacedict_entry, &ifacedict_kv);

            /* printf("%d", len); */
            dbus_message_iter_get_basic(&ifacedict_kv, &iface);
            /* printf("    %s\n",iface); */
            if (strncmp(iface, "org.bluez.Device1", 16) == 0) {
                dbus_message_iter_next(&ifacedict_kv);
                int len = dbus_message_iter_get_element_count(&ifacedict_kv);
                DBusMessageIter propdict_entry, propdict_kv;
                dbus_message_iter_recurse(&ifacedict_kv, &propdict_entry);
                /* char *devname; */
                int connected = 0;
                while(len-- > 0) {
                    dbus_message_iter_recurse(&propdict_entry, &propdict_kv);
                    dbus_message_iter_get_basic(&propdict_kv, &prop);
                    /* printf("        %s\n", prop); */
                    DBusMessageIter variant;
                    if (strncmp(prop, "Connected", 9) == 0) {
                        dbus_message_iter_next(&propdict_kv);
                        dbus_message_iter_recurse(&propdict_kv, &variant);
                        dbus_message_iter_get_basic(&variant, &connected);

                    } /* else if (strncmp(prop, "Name", 4) == 0) { */
                    /*     dbus_message_iter_next(&propdict_kv); */
                    /*     dbus_message_iter_recurse(&propdict_kv, &variant); */
                    /*     dbus_message_iter_get_basic(&variant, &devname); */
                    /* } */
                    dbus_message_iter_next(&propdict_entry);
                }
                if (connected) {
                    printf("⇋");
                    return 0;
                }
            }
            dbus_message_iter_next(&ifacedict_entry);
        }
        // Go to next dict entry
        dbus_message_iter_next(&pathdict_entry);
    }
    dbus_message_unref(msg_reply);

    printf("%%{F#665C54}⇋%%{F-}");
    return 0;
}

static void check_and_abort(DBusError *error) {
    if (!dbus_error_is_set(error)) return;
    puts(error->message);
    dbus_error_free(error);
    abort();
}
