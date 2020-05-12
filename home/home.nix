{ config, lib, pkgs, ... }:
let
  fetchurl = _:
    pkgs.fetchurl {
      url = "https://public-cdn.cloud.unity3d.com/hub/prod/UnityHub.AppImage";
      sha256 = "09nrgjlknl3hgrrl7rc79bmbrq6r6sl49dw0cmvs37vjqnvlr8ny";
    };
  unityhub-fixed = pkgs.unityhub.override (_: { inherit fetchurl; });
in {
  imports = [
    ../overlays.nix
    ./qute.nix
    ./emacs.nix
    ./term/term.nix
    ./bspwm
    ./email.nix
    ./git.nix
    ./mpv.nix
    ./newsboat.nix
    ./polybar.nix
    ./sxhkd.nix
    ./zsh.nix
    ./audacity.nix
  ];
  gtk = {
    enable = true;
    theme.package = pkgs.plasma5.breeze-gtk;
    theme.name = "Breeze-Dark";
    iconTheme.package = pkgs.paper-icon-theme;
    iconTheme.name = "Paper";
  };
  qt.platformTheme = "gtk";
  services.xcape = {
    enable = true;
    mapExpression = { Control_L = "Escape"; };
  };
  services.kdeconnect = {
    enable = true;
    indicator = true;
  };
  services.picom = {
    enable = true;
    fade = true;
    fadeDelta = 3;
    vSync = true;
    extraOptions = ''
      corner-radius = 12.0;
      detect-rounded-corners = true;
      rounded-corners-exclude = [
        "window_type = 'menu'",
        "window_type = 'dropdown_menu'",
        "window_type = 'popup_menu'",
        "window_type = 'utility'",
        "class_g = 'Polybar'",
        "class_g = 'Rofi'",
        "class_g = 'Dunst'"
      ];
    '';
    blur = true;
    experimentalBackends = true;
    #activeOpacity = "0.92";
    #inactiveOpacity = "0.92";
  };
  services.syncthing.enable = true;
  xresources.properties = {
    "*.font" = "FiraCode Nerd Font:pixelsize=18:antialias=true:autohint=true;";
    "*.alpha" = "100.0";
  };
  xresources.extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
    owner = "base16-templates";
    repo = "base16-xresources";
    rev = "d762461de45e00c73a514408988345691f727632";
    sha256 = "08msc3mgf1qzz6j82gi10fin12iwl2zh5annfgbp6nkig63j6fcx";
  } + "/xresources/base16-macintosh-256.Xresources");
  home.packages = with pkgs; [
    ffmpeg-full
    discord
    kotatogram-desktop
    antibody
    gopass
    qutebrowser
    dmenu
    vim
    spotify
    teams
    nodejs
    kotatogram-desktop
    hexchat
    anki
    mu
    hunspell
    hunspellDicts.en-gb-large
    texlive.combined.scheme-full

    calibre

    # rust userutils
    cargo
    rustc
    bat
    exa

    toggldesktop

    neovim

    acpi
    sxiv
    deluge
    unityhub-fixed
    kdenlive
    alex-npm-packages.nativefier
  ];
  programs.zathura.enable = true;

  services.random-background = {
    enable = true;
    imageDirectory = "%h/images/wallpapers";
  };
  home.keyboard = {
    layout = "us";
    variant = "dvp";
    options = [ "ctrl:nocaps" ];
  };
  programs.beets = {
    enable = true;
    settings = { "directory" = "/data/music"; };
  };
  home.file.setaswallpaper = let
    x = builtins.fetchurl
      "https://store.kde.org/p/1169583/startdownload?file_id=1578608483";
  in {
    source = "${x}";
    target = ".local/share/kservices5/ServiceMenus/SetAsWallpaper.desktop";
  };
  xsession.enable = true;
  services.redshift = {
    enable = true;
    latitude = "52.0";
    longitude = "0.0";
  };
  programs.tmux = {
    enable = true;
    keyMode = "vi";
    shortcut = "a";
    plugins = with pkgs; [ tmuxPlugins.sensible tmuxPlugins.yank ];
  };

}
