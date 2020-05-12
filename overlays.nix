{ config, pkgs, lib, fetchFromGitHub, ... }: {
  nixpkgs.config = {
    packageOverrides = pkgs: {
      st = pkgs.st.overrideDerivation (attrs: {
        src = builtins.fetchurl {
          url = "https://github.com/Lukesmithxyz/st/archive/master.tar.gz";
        };
      });
    };
  };
  nixpkgs.overlays = [ (import ./pkgs/overlay.nix) ];
  nixpkgs.config.allowUnfree = true;
}
