#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <dirent.h>
#include <string.h>


int read_int(const char* filename) {
    int energy;
    FILE *f = fopen(filename,"r");
    fscanf(f,"%d",&energy);
    return energy;
}

int get_joint_percent() {
    /* float energy0 = read_int("/sys/class/power_supply/BAT0/energy_now"); */
    /* float maxenergy0 = read_int("/sys/class/power_supply/BAT0/energy_full"); */

    /* float energy1 = read_int("/sys/class/power_supply/BAT1/energy_now"); */
    /* float maxenergy1 = read_int("/sys/class/power_supply/BAT1/energy_full"); */

    /* return (int)roundf((energy1+energy0) / (maxenergy1+maxenergy0) * 100); */
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

char * get_icon () {
    if (read_int("/sys/class/power_supply/AC/online")) {
        return "";
    }
    return "";
}

int main () {
    printf("%s %d", get_icon(), get_joint_percent());
    return 0;
}
