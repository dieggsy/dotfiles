DEST=~/bin/blocks/
CXXFLAGS=-O3

FILES=$(wildcard *.c)
TARGETS=$(basename $(FILES))

all: $(TARGETS) darksky

small: CXXFLAGS += -s
small: all

clean:
	rm -rvf $(TARGETS) darksky

networkmanager: CXXFLAGS += -lsystemd

bluez: CXXFLAGS += -lsystemd

battery-average: CXXFLAGS += -lm

intel-backlight: CXXFLAGS += -lm

darksky : darksky.py
	cp darksky.py darksky

% : %.c
	$(CC) $(CPPFLAGS) $(CXXFLAGS) -o $@ $<
