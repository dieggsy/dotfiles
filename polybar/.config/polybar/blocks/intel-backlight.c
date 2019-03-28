#include <stdio.h>
#include <math.h>

int main () {
    float curr;
    FILE *f;
    f = fopen("/sys/class/backlight/intel_backlight/brightness", "r");
    fscanf(f,"%f", &curr);

    float max;
    f = fopen("/sys/class/backlight/intel_backlight/max_brightness", "r");
    fscanf(f,"%f", &max);

    printf("%d",(int)roundf(curr/max * 100));
    return 0;
}
