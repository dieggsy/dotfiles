.DEFAULT_GOAL:=install

CFLAGS=-O3 -Wall -Wextra

_CONF=$(filter-out polybar/ usr/ etc/ bin/,$(sort $(wildcard */)))
CONF=$(_CONF:%/=%)

_ETC=$(filter-out etc/iwd/,$(wildcard etc/*/))
ETC=$(_ETC:etc/%/=%)

_USR=$(wildcard usr/*/)
USR=$(_USR:usr/%/=%)

.PHONY: install
install: conf etc bin usr

.PHONY: conf
conf: polybar
	stow -t ~ $(CONF)

.PHONY: bin
bin: bin/bin/git-prompt
	stow -t ~ bin

git-prompt: bin/bin/git-prompt.c
	$(CC) $(CFLAGS) bin/bin/git-prompt.c -o bin/bin/git-prompt \
		`pkg-config --silence-errors --libs --cflags libgit2`

.PHONY: etc
etc: iwd
	cd etc/ && sudo stow -t /etc $(ETC)

.PHONY: usr
usr:
	cd usr/ && sudo stow -t /usr $(USR)

.PHONY: polybar
polybar:
	cd polybar/.config/polybar/blocks/ && make
	stow -t ~ polybar

.PHONY: iwd
iwd:
	sudo cp etc/iwd/iwd/main.conf /etc/iwd/main.conf

.PHONY: $(CONF)
$(CONF): $(_CONF)
	stow -t ~ $@

.PHONY: $(ETC)
$(ETC): $(_ETC)
	cd etc && sudo stow -t /etc $@

.PHONY: $(USR)
$(USR): $(_USR)
	cd usr && sudo stow -t /usr $@
