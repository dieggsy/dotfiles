.DEFAULT_GOAL:=install

_CONF=$(filter-out polybar/ etc/ bin/,$(sort $(wildcard */)))
CONF=$(_CONF:%/=%)

_ETC=$(wildcard etc/*/)
ETC=$(_ETC:etc/%/=%)

.PHONY: install
install: conf etc bin

.PHONY: conf
conf: polybar
	stow -t ~ $(CONF)

.PHONY: bin
bin:
	stow -t ~ bin

.PHONY: etc
etc:
	cd etc/ && stow -t /etc $(ETC)

.PHONY: polybar
polybar:
	cd polybar/.config/polybar/blocks/ && make
	stow -t ~ polybar

.PHONY: $(CONF)
$(CONF): $(_CONF)
	stow -t ~ $@

.PHONY: $(ETC)
$(ETC): $(_ETC)
	cd etc && stow -t /etc $@
