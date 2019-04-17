DEST=~/bin/blocks/
CFLAGS=-O3 -Wall -Wextra -pedantic

FILES=$(wildcard *.c)
TARGETS=$(basename $(FILES))

all: $(TARGETS)

small: CFLAGS += -s
small: all

clean:
	rm -rvf $(TARGETS)

networkmanager: CFLAGS += -lsystemd

bluez: CFLAGS += -lsystemd

battery-average: CFLAGS += -lm -lsystemd -lespeak-ng

intel-backlight: CFLAGS += -lm

% : %.c
	$(CC) $(CFLAGS) -o $@ $<
