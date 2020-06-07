{ config, pkgs, lib, ... }: {
  nixpkgs.config.packageOverrides = pkgs: {
    st = pkgs.st.overrideDerivation (attrs: {
      src = builtins.fetchurl {
        url = "https://github.com/Lukesmithxyz/st/archive/master.tar.gz";
      };
    });
  };
  nixpkgs.config.permittedInsecurePackages = [ "p7zip-16.02" ];
  nixpkgs.overlays = [
    (import ./pkgs/overlay.nix)
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
  nixpkgs.config.allowUnfree = true;
}
