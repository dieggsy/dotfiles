DEST=~/bin/blocks/
CFLAGS=-O3 -Wall -Wextra -pedantic

FILES=$(wildcard *.c)
TARGETS=$(basename $(FILES))

all: $(TARGETS)

small: CFLAGS += -s
small: all

clean:
	rm -rvf $(TARGETS)

networkmanager: CFLAGS += `pkg-config --cflags --libs libnm`

bluez: CFLAGS += `pkg-config --cflags --libs dbus-1`

battery-average: CFLAGS += -lm `pkg-config --libs espeak-ng 2>/dev/null || echo -lespeak` `pkg-config --cflags --libs dbus-1`

intel-backlight: CFLAGS += -lm

% : %.c
	$(CC) $(CFLAGS) -o $@ $<
