.DEFAULT_GOAL:=install

CFLAGS=-O3 -Wall -Wextra

_CONF=$(filter-out usr/ etc/ bin/,$(sort $(wildcard */)))
CONF=$(_CONF:%/=%)

_ETC=$(wildcard etc/*/)
ETC=$(_ETC:etc/%/=%)

_USR=$(wildcard usr/*/)
USR=$(_USR:usr/%/=%)

.PHONY: install
install: conf etc bin usr

.PHONY: conf
conf:
	stow -t ~ $(CONF)

.PHONY: bin
bin: bin/bin/git-prompt
	stow -t ~ bin

git-prompt: bin/bin/git-prompt.c
	$(CC) $(CFLAGS) bin/bin/git-prompt.c -o bin/bin/git-prompt \
		`pkg-config --silence-errors -libs --cflags libgit2`

.PHONY: etc
etc:
	cd etc/ && sudo stow -t /etc $(ETC)

.PHONY: usr
usr:
	cd usr/ && sudo stow -t /usr $(USR)

.PHONY: $(CONF)
$(CONF): $(_CONF)
	stow -t ~ $@

.PHONY: $(ETC)
$(ETC): $(_ETC)
	cd etc && sudo stow -t /etc $@

.PHONY: $(USR)
$(USR): $(_USR)
	cd usr && sudo stow -t /usr $@
