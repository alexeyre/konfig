{ config, pkgs, lib, mkDerivation, qtbase, qttools, ... }:
let 
  homeManagerTarball = 
    fetchTarball
      https://github.com/rycee/home-manager/archive/master.tar.gz;
in
{
  nixpkgs.config = {
    packageOverrides = pkgs: {
      home-manager-unstable = import homeManagerTarball {};
      st = pkgs.st.overrideDerivation(attrs: {
        src = builtins.fetchurl {
          url = "https://github.com/Lukesmithxyz/st/archive/master.tar.gz";
        };
      });
    };
  };
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
    (import ./pkgs/overlay.nix)
  ];
  nixpkgs.config.allowUnfree = true;
}
