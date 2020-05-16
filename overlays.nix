{ config, pkgs, lib, fetchFromGitHub, ... }: {
  nixpkgs.config.packageOverrides = pkgs: {
    st = pkgs.st.overrideDerivation (attrs: {
      src = builtins.fetchurl {
        url = "https://github.com/Lukesmithxyz/st/archive/master.tar.gz";
      };
    });
  };
  nixpkgs.overlays = [
    (import ./home/anki)
    (import ./pkgs/overlay.nix)
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
  nixpkgs.config.allowUnfree = true;
}
