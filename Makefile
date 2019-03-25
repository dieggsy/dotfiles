.DEFAULT_GOAL:=install

_CONFIG=$(filter-out etc/,$(sort $(wildcard */)))
CONFIG=$(_CONFIG:%/=%)

_ETC=$(wildcard etc/*/)
ETC=$(_ETC:etc/%/=%)

.PHONY: install
install: install-conf install-etc

.PHONY: install-conf
install-conf:
	stow -t ~ $(CONFIG)

.PHONY: install-etc
install-etc:
	cd etc/ && stow -t /etc $(ETC)

.PHONY: $(CONFIG)
$(CONFIG): $(_CONFIG)
	stow -t ~ $@

.PHONY: $(ETC)
$(ETC): $(_ETC)
	cd etc && stow -t /etc $@
