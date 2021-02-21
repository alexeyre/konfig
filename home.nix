{ config, pkgs, ... }: {
  imports = [ ./shell.nix ./emacs ./email.nix ];
  xdg.enable = true;
  programs.chromium = {
    enable = false;
    package = pkgs.ungoogled-chromium;
    extensions = [
      { id = "cgbcahbpdhpcegmbfconppldiemgcoii"; } # ublock dev
      { id = "eckgcipdkhcfghnmincccnhpdmnbefki"; } # umatrix dev
      { id = "kkhfnlkhiapbiehimabddjbimfaijdhk"; } # gopass bridge
      { id = "ohnjgmpcibpbafdlkimncjhflgedgpam"; } # 4chanX
    ];
  };
  home.packages = with pkgs; [
    (texlive.combine { inherit (texlive) scheme-medium wrapfig ulem amsmath capt-of hyperref; } )
    niv
    gopass
    nixfmt
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
  programs.tmux = {
    keyMode = "vi";
    newSession = true;
    prefix = "C-a";
  };
}
