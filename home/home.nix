{ config, lib, pkgs, ... }:
let
  fetchurl = _: pkgs.fetchurl {
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
    ./bspwm.nix
    ./email.nix
    ./git.nix
    ./mpv.nix
    ./newsboat.nix
    ./polybar.nix
    ./sxhkd.nix
    ./zsh.nix
  ];
  services.dunst.enable = true;
  
  services.udiskie.enable = true;
  gtk = {
    enable = true;
    theme.package = pkgs.arc-theme;
    theme.name = "Arc-Dark";
    iconTheme.package = pkgs.arc-icon-theme;
    iconTheme.name = "Arc";
  };
  services.xcape = {
    enable = true;
    mapExpression = { Control_L = "Escape"; };
  };
  services.picom = {
    enable = true;
    fade = true;
    fadeDelta = 2;
    #activeOpacity = "0.92";
    #inactiveOpacity = "0.92";
  };
  services.syncthing.enable = true;
  xresources.properties = {
    "*.font" = "FiraCode Nerd Font:pixelsize=18:antialias=true:autohint=true;";
    "*.alpha" = "100.0";
  };
  xresources.extraConfig = builtins.readFile (
    pkgs.fetchFromGitHub {
      owner = "base16-templates";
      repo = "base16-xresources";
      rev = "d762461de45e00c73a514408988345691f727632";
      sha256 = "08msc3mgf1qzz6j82gi10fin12iwl2zh5annfgbp6nkig63j6fcx";
    } + "/xresources/base16-macintosh-256.Xresources"
  );
  programs.obs-studio.enable = true;
  programs.pidgin = {
    enable = true;
    plugins = [ pkgs.telegram-purple ];
  };
  home.packages = with pkgs; [
    ffmpeg-full
    python38Packages.grip
    light
    discord-canary
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
    nativefier

  ];
  programs.zathura.enable = true;
   
  services.random-background = {
    enable = true;
    imageDirectory = "%h/images/wallpapers";
  };
  services.keybase.enable = true;
  home.keyboard = {
    layout = "us";
    variant = "dvp";
    options = ["ctrl:nocaps"];
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
