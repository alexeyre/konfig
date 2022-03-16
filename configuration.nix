{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ./keyboard ./vi ./shell.nix ./yabai.nix ];
  networking.hostName = "fedbook";
  nix.trustedUsers = [ "alex" ];
  services.nix-daemon.enable = true;
  nixpkgs.system = "aarch64-darwin";
  nix.extraOptions = ''
    build-users-group = nixbld
    experimental-features = nix-command flakes ca-references
  '';

  environment.darwinConfig = "$HOME/.config/nix/configuration.nix";

  # ugly hack
  users.users.alex.home = "/Users/alex";

  # Nix tooling options
  nix.package = pkgs.nixUnstable; # Needed for nix flakes
  nixpkgs.config.allowUnfree = true; # Allow the installation of unfree packages

  # Enter home-configuration
  home-manager.users.alex = { ... }: {
    imports = [ ./brew ./alfred ];

    programs.brew.enable = true;
    programs.brew.taps = [
      "candid82/brew"
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/core"
      "homebrew/services"
      "homebrew/cask-fonts"
    ];

    programs.brew.casks = [ "font-terminus" "iterm2" ];
    # Enable the use of XDG directories, see https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
    xdg.enable = true;

    ###########
    ### Git ###
    ###########
    programs.gh.enable = false; # Enable GitHub specific tooling
    programs.git.lfs.enable = true; # Enable Git LFS
    programs.git = {
      enable = true;
      userName = "Alex Eyre";
      userEmail = "alexeeyre@gmail.com";
      signing = {
        signByDefault = true;
        key = "954EBF489FD70E2E4694082B3E1F5A8C0C4F9FB3";
        gpgPath = "${pkgs.gnupg}/bin/gpg";
      };
      extraConfig.pull = {
        rebase = true;
        merge = false;
      };
    };

    programs.readline = {
      enable = true;
      variables."bell-style" = "none";
    };

    home.packages = with pkgs; [
      gopass
      (writeScriptBin "strip_exif" ''
        #!${pkgs.stdenv.shell}
        ${pkgs.exiftool}/bin/exiftool -all= "$@"
      '')
      less
      (writeScriptBin "youtube-dl_wav" ''
        ${pkgs.youtube-dl}/bin/youtube-dl --ffmpeg-location=${pkgs.ffmpeg}/bin/ffmpeg -x --audio-format=wav $1
      '')
      (writeScriptBin "download_wallpaper" ''
        ${pkgs.curl}/bin/curl -l $1 -o ~/Pictures/papes/$(${pkgs.coreutils}/bin/basename $1)
      '')
      # (texlive.combine { inherit (texlive) plantuml minted fvextra scheme-full wrapfig ulem amsmath capt-of hyperref; })
      (pkgs.writeScriptBin "compress_video" ''
        #!${pkgs.stdenv.shell}
            file_name="''${1##*/}"
            file_name="''${file_name%.*}"
            file_name="''${file_name}.mp4"
            dir_name="$(dirname $1)"
            full_path="''${dir_name}/''${file_name}"
            test "$(file -i $1 | grep video)" = "video" && echo ${pkgs.ffmpeg} -i $1 -vcodec libx265 -crf 28 -tag:v hvc1 "''${full_path}"
          '')
    ];
  };
}
