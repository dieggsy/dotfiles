DEST=~/bin/blocks/
CFLAGS=-O3 -Wall -Wextra -pedantic

FILES=$(wildcard *.c)
TARGETS=$(basename $(FILES))

all: $(TARGETS)

small: CFLAGS += -s
small: all

clean:
	rm -rvf $(TARGETS)

networkmanager: CFLAGS += `pkg-config --cflags --libs dbus-1`

bluez: CFLAGS += `pkg-config --cflags --libs dbus-1`

battery-average: CFLAGS += -lm -lespeak-ng `pkg-config --cflags --libs dbus-1`

intel-backlight: CFLAGS += -lm

% : %.c
	$(CC) $(CFLAGS) -o $@ $<
