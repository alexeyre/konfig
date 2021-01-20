{ config, pkgs, ... }: {
  # nixpkgs.overlays = [
  #   (import (builtins.fetchTarball {
  #     url =
  #       "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
  #   }))
  #   (import (builtins.fetchTarball
  #     "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz"))
  # ];
  imports = [ ./zsh.nix ./emacs ];
  programs.home-manager.enable = true;
  home.packages = with pkgs; [
    (haskellPackages.ghcWithPackages (pkgs: [ pkgs.QuickCheck pkgs.brittany ]))
    gopass
    nixfmt
    (texlive.combine { inherit (texlive) scheme-minimal dvipng; })
    (pkgs.writeScriptBin "youtube-dl_wav" ''
${pkgs.youtube-dl}/bin/youtube-dl --ffmpeg-location=${pkgs.ffmpeg}/bin/ffmpeg -x --audio-format=wav $1
'')
  ];
  programs.git = {
    enable = true;
    userName = "Alex Eyre";
    userEmail = "hi@alexey.re";
    extraConfig.http.sslVerify = false;
  };
}
