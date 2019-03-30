DEST=~/bin/blocks/
CFLAGS=-O3 -Wall -Wextra -pedantic

FILES=$(wildcard *.c)
TARGETS=$(basename $(FILES))

all: $(TARGETS) darksky

small: CFLAGS += -s
small: all

clean:
	rm -rvf $(TARGETS) darksky

networkmanager: CFLAGS += -lsystemd

bluez: CFLAGS += -lsystemd

battery-average: CFLAGS += -lm

intel-backlight: CFLAGS += -lm

darksky : darksky.py
	ln -sf darksky.py darksky

% : %.c
	$(CC) $(CFLAGS) -o $@ $<
