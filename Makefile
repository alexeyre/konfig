.PHONY: help
.PHONY: format
.PHONY: clean
.PHONY: install

HOSTNAME := $(shell hostname)

help:           ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'

format:
	nix-shell -p nixfmt findutils --command 'find . -type f -name "*.nix" -exec nixfmt {} \;'
install:
	ln -s $(PWD)/systems/$(HOSTNAME).nix ~/.config/nix/configuration.nix
	darwin-rebuild -I darwin-config=$(HOME)/.config/nix/configuration.nix switch
clean:
		rm -f ~/.config/nix/configuration.nix
