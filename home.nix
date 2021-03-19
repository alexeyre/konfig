{ config, pkgs, ... }: {
  imports = [ ./modules ];
  xdg.enable = true;
  alex.vi = true;
  alex.chromium.enable = false;
  alex.shell.enable = true;
  home.packages = with pkgs; [
    niv
    gopass
    nixfmt
    (writeScriptBin "strip_exif" ''
      #!${pkgs.stdenv.shell}
      ${pkgs.exiftool}/bin/exiftool -all= "$@"
    '')
    (writeScriptBin "youtube-dl_wav" ''
      ${pkgs.youtube-dl}/bin/youtube-dl --ffmpeg-location=${pkgs.ffmpeg}/bin/ffmpeg -x --audio-format=wav $1
    '')
    (writeScriptBin "download_wallpaper" ''
      ${pkgs.curl}/bin/curl -l $1 -o ~/Pictures/papes/$(${pkgs.coreutils}/bin/basename $1)
    '')
  ];
  programs.git = {
    enable = true;
    userName = "Alex Eyre";
    userEmail = "alexeeyre@gmail.com";
    # extraConfig.http.sslVerify = false;
  };
}
