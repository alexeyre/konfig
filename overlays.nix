{ config, pkgs, ... }:
let 
unstableNixosTarball = 
	fetchTarball
		https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
unstableNixpkgsTarball =
	fetchTarball
		https://github.com/NixOS/nixpkgs-channels/archive/nixpkgs-unstable.tar.gz;
homeManagerTarball = 
	fetchTarball
		https://github.com/rycee/home-manager/archive/master.tar.gz;
in
{
	nixpkgs.config = {
		packageOverrides = pkgs: {
			nixpkgs = import unstableNixpkgsTarball {
				config = config.nixpkgs.config;
			};
			nixos = import unstableNixosTarball {
				config = config.nixos.config;
			};
			home-manager-unstable = import homeManagerTarball {};
		};
	};
	nixpkgs.overlays = [
		(import (builtins.fetchTarball {
			 url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
			 }))
	(import (builtins.fetchTarball {
		 url = https://github.com/colemickens/nixpkgs-wayland/archive/master.tar.gz;
		 }))
	(import ./pkgs/overlay.nix)
	];
}
