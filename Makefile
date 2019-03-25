.DEFAULT_GOAL:=install

_CONF=$(filter-out etc/ bin/,$(sort $(wildcard */)))
CONF=$(_CONF:%/=%)

_ETC=$(wildcard etc/*/)
ETC=$(_ETC:etc/%/=%)

.PHONY: install
install: conf etc bin

.PHONY: conf
conf:
	stow -t ~ $(CONF)

.PHONY: bin
bin:
	stow -t ~ bin

.PHONY: etc
etc:
	cd etc/ && stow -t /etc $(ETC)

.PHONY: $(CONF)
$(CONF): $(_CONF)
	stow -t ~ $@

.PHONY: $(ETC)
$(ETC): $(_ETC)
	cd etc && stow -t /etc $@
