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
  xdg.enable = true;
  home.packages = with pkgs; [
    gopass
    nixfmt
    (pkgs.writeScriptBin "youtube-dl_wav" ''
${pkgs.youtube-dl}/bin/youtube-dl --ffmpeg-location=${pkgs.ffmpeg}/bin/ffmpeg -x --audio-format=wav $1
'')
(pkgs.writeScriptBin "download_wallpaper" ''
${pkgs.curl}/bin/curl -l $1 -o ~/Pictures/papes/$(${pkgs.coreutils}/bin/basename $1)
'')
  ];
  programs.git = {
    enable = true;
    userName = "Alex Eyre";
    userEmail = "hi@alexey.re";
    extraConfig.http.sslVerify = false;
  };
}
