.PHONY: help
.PHONY: format
.PHONY: clean
.PHONY: install
.PHONY: switch

.DEFAULT_GOAL := all
HOSTNAME := $(shell hostname)
PWD := $(shell pwd)

all: format build switch
format:
	nix-shell -p nixfmt findutils --command 'find . -type f -name "*.nix" -exec nixfmt {} \;'
switch:
	./result/sw/bin/darwin-rebuild switch --flake .

build:
	nix build "$(PWD)#darwinConfigurations.fedbook.system"
debug:
	nix build --show-trace -v "$(PWD)#darwinConfigurations.fedbook.system"

upgrade:
	nix flake update && git commit . -m "meta: bump flake"
