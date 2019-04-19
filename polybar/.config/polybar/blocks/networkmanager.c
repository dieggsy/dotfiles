#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dbus/dbus.h>

static int string_has_suffix(const char* suffix, const char* str);
static void check_and_abort(DBusError *error);
static DBusMessage *get_property(DBusConnection *connection,
                                 const char *bus_name,
                                 const char *path,
                                 const char *iface,
                                 const char *propname);
static void *get_basic_property(DBusConnection *conn,
                                const char *target,
                                const char *object,
                                const char *iface,
                                const char *prop);
static DBusMessage *nm_get_active_connections(DBusConnection *conn);
static int nm_vpn_connected(DBusConnection *conn);

int main() {
    DBusError error;
    dbus_error_init(&error);

    // Initialize dbus connection
    DBusConnection *connection = dbus_bus_get(DBUS_BUS_SYSTEM, &error);
    check_and_abort(&error);

    const char *interface_name = "org.freedesktop.NetworkManager";

    // Bool - check if there is a vpn connected
    int vpn_connected = nm_vpn_connected(connection);

    // Get variant of array of paths of active connections
    DBusMessage *msg_reply = nm_get_active_connections(connection);

    // Start iter to 'enter' variant type
    DBusMessageIter args;
    dbus_message_iter_init(msg_reply, &args);
    // Enter the variant
    DBusMessageIter sub;
    dbus_message_iter_recurse(&args, &sub);
    //Enter the array
    DBusMessageIter subsub;
    dbus_message_iter_recurse(&sub, &subsub);


    char* path, *icon, *id, *type;
    int empty = 1;

    // Array length, presumably
    int len = dbus_message_iter_get_element_count(&sub);
    // Iterate over all connection paths
    while (len-- > 0) {
        dbus_message_iter_get_basic(&subsub, &path);
        type = get_basic_property(connection,
                                  interface_name,
                                  path,
                                  "org.freedesktop.NetworkManager.Connection.Active",
                                  "Type");
        if (string_has_suffix("wireless", type)) {
            icon = vpn_connected ? "" : "";
        } else if (string_has_suffix("ethernet", type)) {
            icon = "";
        } else if (string_has_suffix("vpn", type)
                   || string_has_suffix("tun", type)) {
            dbus_message_iter_next(&subsub);
            continue;
        } else  {
            icon = "?";
        }

        id = get_basic_property(connection,
                                interface_name,
                                path,
                                "org.freedesktop.NetworkManager.Connection.Active",
                                "Id");

        if (!empty) {
            fputs(" ", stdout);
        }

        fputs(icon, stdout);
        fputs(" ", stdout);
        fputs(id, stdout);
        empty = 0;
        dbus_message_iter_next(&subsub);

    }

    // Free the reply
    dbus_message_unref(msg_reply);

    if (empty) {
        fputs(" No Connection",stdout);
    }

    return 0;
}

static int string_has_suffix(const char* suffix, const char* str) {
    int len = strlen(str);
    int slen = strlen(suffix);
    return (strncmp(suffix,str + (len - slen), slen) == 0) ? 1 : 0;
}

static void check_and_abort(DBusError *error) {
    if (!dbus_error_is_set(error)) return;
    puts(error->message);
    dbus_error_free(error);
    abort();
}

static DBusMessage *get_property(DBusConnection *connection,
                                 const char *bus_name,
                                 const char *path,
                                 const char *iface,
                                 const char *propname) {

    DBusMessage *msg_query = dbus_message_new_method_call(bus_name, path,
                                                          "org.freedesktop.DBus.Properties",
                                                          "Get");
    dbus_message_append_args(msg_query,
                             DBUS_TYPE_STRING, &iface,
                             DBUS_TYPE_STRING, &propname,
                             DBUS_TYPE_INVALID);

    DBusError error;
    dbus_error_init(&error);
    DBusMessage *msg_reply = dbus_connection_send_with_reply_and_block(connection, msg_query, -1, &error);
    check_and_abort(&error);
    dbus_message_unref(msg_query);
    return msg_reply;
}

static void *get_basic_property(DBusConnection *conn,
                                const char *target,
                                const char *object,
                                const char *iface,
                                const char *prop) {

    DBusMessage *reply = get_property(conn, target, object, iface, prop);
    if (reply == NULL)
        return NULL;

    DBusMessageIter args;
    dbus_message_iter_init(reply, &args);

    DBusMessageIter sub;
    dbus_message_iter_recurse(&args, &sub);
    void *res;
    dbus_message_iter_get_basic(&sub, &res);
    dbus_message_unref(reply);
    return res;
}

static DBusMessage *nm_get_active_connections(DBusConnection *conn) {
    DBusMessage *msg_reply = get_property(conn,
                                          "org.freedesktop.NetworkManager",
                                          "/org/freedesktop/NetworkManager",
                                          "org.freedesktop.NetworkManager",
                                          "ActiveConnections");
    return msg_reply;
}

static int nm_vpn_connected(DBusConnection *conn) {
    DBusMessage *msg_reply = nm_get_active_connections(conn);
    // Start iter to 'enter' variant type
    DBusMessageIter args;
    dbus_message_iter_init(msg_reply, &args);
    // Enter the variant
    DBusMessageIter sub;
    dbus_message_iter_recurse(&args, &sub);
    //Enter the array
    DBusMessageIter subsub;
    dbus_message_iter_recurse(&sub, &subsub);

    // Array length, presumably
    int len = dbus_message_iter_get_element_count(&sub);

    char *path, *type;
    while (len-- > 0) {
        dbus_message_iter_get_basic(&subsub, &path);
        /* printf("Path: %s\n", path); */
        type = get_basic_property(conn,
                                  "org.freedesktop.NetworkManager",
                                  path,
                                  "org.freedesktop.NetworkManager.Connection.Active",
                                  "Type");
        if (string_has_suffix("vpn", type)
            || string_has_suffix("tun", type)) {
            return 1;
        }
    }
    dbus_message_unref(msg_reply);
    return 0;
}
