{ config, pkgs, ... }: {
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
  programs.tmux = {
    enable = true;
    package = pkgs.hello;
    keyMode = "vi";
    newSession = true;
    prefix = "C-a";
  };
}
