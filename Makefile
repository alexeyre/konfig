.PHONY: help
.PHONY: format
.PHONY: clean
.PHONY: install
.PHONY: switch

.DEFAULT_GOAL := switch
HOSTNAME := $(shell hostname)
PWD := $(shell pwd)

all: clean install switch
format:
	nix-shell -p nixfmt findutils --command 'find . -type f -name "*.nix" -exec nixfmt {} \;'
switch:
	nix build "$(PWD)#darwinConfigurations.fedbook.system"
	./result/sw/bin/darwin-rebuild switch --flake .
upgrade:
	nix flake update && git commit . -m "meta: bump flake"
