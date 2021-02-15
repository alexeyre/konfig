{ config, pkgs, ... }: {
  imports = [ ./zsh.nix ./emacs ];
  xdg.enable = true;
  programs.chromium = {
    enable = true;
    package = pkgs.ungoogled-chromium;
    extensions = [
      { id = "cgbcahbpdhpcegmbfconppldiemgcoii"; } # ublock dev
      { id = "eckgcipdkhcfghnmincccnhpdmnbefki"; } # umatrix dev
      { id = "kkhfnlkhiapbiehimabddjbimfaijdhk"; } # gopass bridge
    ];
  };
  home.packages = with pkgs; [
    niv
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
    userEmail = "alexeeyre@gmail.com";
    # extraConfig.http.sslVerify = false;
  };
  programs.tmux = {
    keyMode = "vi";
    newSession = true;
    prefix = "C-a";
  };
}
