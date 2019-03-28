#include <stdio.h>
#include <string.h>
#include <systemd/sd-bus.h>

int main () {
    int r;

    sd_bus *bus=NULL;
    sd_bus_message *msg = NULL;
    sd_bus_error error = SD_BUS_ERROR_NULL;

    r = sd_bus_open_system(&bus);
    if (r < 0) {
        fputs("Failed to connect to system bus\n", stderr);
        return r;
    }

    // Get active connections
    r = sd_bus_call_method(bus,
                           "org.bluez",
                           "/",
                           "org.freedesktop.DBus.ObjectManager",
                           "GetManagedObjects", &error, &msg, "" /* "a{oa{sa{sv}}}" */);
    if (r < 0) {
        fprintf(stderr,"Failed to read managed objects: %s - %s\n", error.name, error.message);
        return r;
    }
    char * icon = "";
    char * name;
    // Start traversing the managed objects to look for connected device
    r = sd_bus_message_enter_container(msg, 'a', "{oa{sa{sv}}}");

    // Traverse path/dict pair
    while (sd_bus_message_enter_container(msg, 'e', "oa{sa{sv}}") > 0){

        char * path;
        sd_bus_message_read(msg, "o", &path);
        /* puts(path); */
        /* r = sd_bus_message_skip(msg, "a{sa{sv}}"); */

        r = sd_bus_message_enter_container(msg, 'a', "{sa{sv}}");
        char * iface;
        // Traverse interface/dict pair
        while (sd_bus_message_enter_container(msg, 'e', "sa{sv}") > 0) {
            sd_bus_message_read(msg, "s", &iface);
            /* fputs("    ",stdout); */
            /* puts(iface); */
            // We only care if the interface is a device
            if (strncmp(iface, "org.bluez.Device1", 16) == 0) {
                r = sd_bus_message_enter_container(msg, 'a', "{sv}");
                char * prop;
                // Traverse property/variant pair
                char * devname;
                int connected=0;
                while (sd_bus_message_enter_container(msg, 'e', "sv") > 0) {
                    sd_bus_message_read(msg, "s", &prop);
                    /* fputs("        ", stdout); */
                    /* puts(prop); */
                    if (strncmp(prop, "Connected", 9) == 0) {
                        sd_bus_message_enter_container(msg, 'v', "b");
                        sd_bus_message_read(msg, "b", &connected);
                        sd_bus_message_exit_container(msg);
                        /* fputs("        ", stdout); */
                        /* puts("Found connected!"); */
                        sd_bus_message_skip(msg, "v");
                    } else if (strncmp(prop, "Name", 4) == 0) {
                        sd_bus_message_enter_container(msg, 'v', "s");
                        sd_bus_message_read(msg, "s", &devname);
                        /* fputs("            ", stdout); */
                        /* puts(devname); */
                        sd_bus_message_exit_container(msg);

                    } else {
                        sd_bus_message_skip(msg, "v");
                    }
                    sd_bus_message_exit_container(msg);
                }
                if (connected) {
                    printf(" %s", devname);
                    return 0;
                }
                sd_bus_message_exit_container(msg);
            } else {
                sd_bus_message_skip(msg, "a{sv}");
            }
            /* sd_bus_message_skip(msg, "a{sv}"); */
            sd_bus_message_exit_container(msg);
        }
        sd_bus_message_exit_container(msg);
        sd_bus_message_exit_container(msg);
    }
    sd_bus_message_exit_container(msg);
    printf("");
    return 0;
}
