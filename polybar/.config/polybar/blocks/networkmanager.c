#include <stdio.h>
#include <string.h>
#include <systemd/sd-bus.h>


int string_has_suffix(const char* suffix, const char* str) {
    int len = strlen(str);
    int slen = strlen(suffix);
    return (strncmp(suffix,str + (len - slen), slen) == 0) ? 1 : 0;
}

int main () {

    int r;

    sd_bus *bus=NULL;
    sd_bus_message *msg = NULL;
    sd_bus_error error = SD_BUS_ERROR_NULL;

    // Connect to system bus
    r = sd_bus_open_system(&bus);
    if (r < 0) {
        fputs("Failed to connect to system bus\n", stderr);
        return r;
    }

    // Get active connections
    r = sd_bus_get_property(bus,
                            "org.freedesktop.NetworkManager",
                            "/org/freedesktop/NetworkManager",
                            "org.freedesktop.NetworkManager",
                            "ActiveConnections", &error, &msg, "ao");
    if (r < 0) {
        fputs("Failed to read active connections\n", stderr);
        return r;
    }

    // Iterate through active connections, storing current object path in path
    r = sd_bus_message_enter_container(msg, 'a', "o");
    // Here, we've 'entered' the array so to speak - as long as message_read
    // doesn't return <= 0, we've gotten another path
    char * path;
    int empty = 1;
    while (sd_bus_message_read(msg, "o", &path) > 0) {

        // Initialize a new message so as to not mes with msg, which we're
        // using to iterate over the active connections array.
        sd_bus_message *nmsg = NULL;
        r = sd_bus_get_property(bus,
                                "org.freedesktop.NetworkManager",
                                path,
                                "org.freedesktop.NetworkManager.Connection.Active",
                                "Type", &error, &nmsg, "s");

        char * type;
        r = sd_bus_message_read_basic(nmsg, 's', &type);

        // Set icon
        char *icon;
        if (string_has_suffix("wireless", type)) {
            icon = "";
        }
        else if (string_has_suffix("ethernet", type)) {
            icon = "";
        }
        else if (string_has_suffix("vpn", type)) {
            continue;
        }
        else if (string_has_suffix("tun", type)) {
            continue;
        }
        else  {
            icon = "?";
        }

        // Get connection name (SSID for wifi, dev name for ethernet)
        char * id;
        r = sd_bus_get_property(bus,
                                "org.freedesktop.NetworkManager",
                                path,
                                "org.freedesktop.NetworkManager.Connection.Active",
                                "Id", &error, &nmsg, "s");
        r = sd_bus_message_read_basic(nmsg, 's', &id);

        // Print
        if (!empty) {
            fputs(" ", stdout);
        }
        fputs(icon, stdout);
        fputs(" ", stdout);
        fputs(id, stdout);
        empty = 0;
    }
    if (empty) {
        puts(" No Connection");
    }
    sd_bus_message_exit_container(msg);
    return 0;
}
