.PHONY: help
.PHONY: format
.PHONY: clean
.PHONY: install
.PHONY: switch

.DEFAULT_GOAL := switch
HOSTNAME := $(shell hostname)
PWD := $(shell pwd)

all: clean install switch

help:           ## Show this help.
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -e 's/\\$$//' | sed -e 's/##//'

format:
	nix-shell -p nixfmt findutils --command 'find . -type f -name "*.nix" -exec nixfmt {} \;'
switch:
	nix build "$(PWD)/systems/$(HOSTNAME)#darwinConfigurations.$(HOSTNAME).system"
	./result/sw/bin/darwin-rebuild switch --flake ./systems/$(HOSTNAME)
switch-debug:
	nix build --show-trace -v "$(PWD)/systems/$(HOSTNAME)#darwinConfigurations.$(HOSTNAME).system"
	./result/sw/bin/darwin-rebuild switch --show-trace -v --flake ./systems/$(HOSTNAME)
