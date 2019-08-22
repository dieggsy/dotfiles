#include <stdio.h>
#include <glib.h>
#include <NetworkManager.h>

char* get_wifi_icon(int strength, int vpn_on);
int round_multiple(int strength, int multiple);

int main () {
    NMClient *client;
	const GPtrArray *devices;
    GError *error = NULL;

    client = nm_client_new (NULL, &error);
    if (!client) {
        g_message ("Error: Could not create NMClient: %s.", error->message);
        g_error_free (error);
        return EXIT_FAILURE;
	}
    int vpn_on = 0;

    const GPtrArray *active = nm_client_get_active_connections(client);
    for (uint i = 0; i < active->len; i++) {
        NMActiveConnection* conn = g_ptr_array_index(active, i);
        if (nm_active_connection_get_vpn(conn)
            || strncmp(nm_active_connection_get_connection_type(conn),"tun",3) == 0) {
            vpn_on = 1;
        }
    }

    devices = nm_client_get_devices (client);
    int have_conn = 0;
    for (uint i = 0; i < devices->len; i++) {
        NMDevice *device = g_ptr_array_index (devices, i);
        if (NM_IS_DEVICE_WIFI(device)) {
            NMAccessPoint *ap = nm_device_wifi_get_active_access_point(NM_DEVICE_WIFI(device));
            if (ap != NULL){
                have_conn = 1;
                GBytes* ssid = nm_access_point_get_ssid(ap);
                char * ssid_str = nm_utils_ssid_to_utf8(g_bytes_get_data (ssid, NULL),
                                                        g_bytes_get_size (ssid));
                printf("%s %s\n",
                       get_wifi_icon(nm_access_point_get_strength(ap),vpn_on),
                       ssid_str);
            }
        } else if (NM_IS_DEVICE_ETHERNET(device)) {
            NMActiveConnection *conn = nm_device_get_active_connection(device);
            if (conn != NULL) {
                have_conn = 1;
                printf("<-> %s\n", nm_active_connection_get_id(conn));
            }
        }
    }
    if (!have_conn || devices->len == 0) {
        printf("! No Connection");
    }

    return 0;
}

char* get_wifi_icon(int strength, int vpn_on) {
    char* inside;
    strength = round_multiple(strength, 33);
    if (strength == 99) {
        inside = vpn_on ? "(>>>)" : "[>>>]";
    } else if (strength == 66) {
        inside = vpn_on ? "(>> )" : "[>> ]";
    } else if (strength == 33) {
        inside = vpn_on ? "(>  )" : "[>  ]";
    } else {
        inside = vpn_on ? "(   )" : "[   ]";
    }
    return inside;
}

int round_multiple(int strength, int multiple) {
    return ((strength + multiple/2)/multiple) * multiple;
}
