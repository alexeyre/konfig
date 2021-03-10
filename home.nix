{ config, pkgs, ... }: {
  imports = [ ./shell.nix ./email.nix ./vi.nix ];
  xdg.enable = true;
  programs.chromium = {
    enable = true;
    extensions = [
      { id = "cgbcahbpdhpcegmbfconppldiemgcoii"; } # ublock dev
      { id = "eckgcipdkhcfghnmincccnhpdmnbefki"; } # umatrix dev
      { id = "kkhfnlkhiapbiehimabddjbimfaijdhk"; } # gopass bridge
      { id = "ohnjgmpcibpbafdlkimncjhflgedgpam"; } # 4chanX
    ];
  };
  home.packages = with pkgs; [
    (texlive.combine {
      inherit (texlive)
        minted fvextra scheme-full wrapfig ulem amsmath capt-of hyperref;
    })
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
  programs.tmux = {
    enable = true;
    keyMode = "vi";
    newSession = true;
    prefix = "C-a";
    package = pkgs.hello;
  };
}
