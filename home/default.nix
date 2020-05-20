{ config, lib, pkgs, ... }: {
  imports = [
    ./qute.nix
    ./emacs
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
    ./rofi
  ];
  gtk = {
    enable = true;
    theme.package = pkgs.arc-theme;
    theme.name = "Arc-Dark";
    iconTheme.package = pkgs.arc-icon-theme;
    iconTheme.name = "Arc";
  };
  xdg.enable = true;
  xdg.mimeApps = {
    associations.added = {
      "image/image" = "sxiv.desktop";
      "application/octet-stream" = "sxiv.desktop";
    };
    enable = true;
  };
  xdg.userDirs = {
    enable = true;
    desktop = "$HOME";
    documents = "$HOME/notes";
    download = "$HOME/downloads";
    music = "$HOME/music";
    pictures = "$HOME/images";
    publicShare = "$HOME";
    templates = "$HOME";
    videos = "$HOME/images/videos";

  };
  qt.platformTheme = "gtk";
  services.xcape = {
    enable = true;
    mapExpression = { Control_L = "Escape"; };
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
  services.syncthing.enable = false;
  xresources.properties = {
    "*.font" = "FiraCode Nerd Font:pixelsize=18:antialias=true:autohint=true;";
    "*.alpha" = "100.0";
  };
  xresources.extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
    owner = "base16-templates";
    repo = "base16-xresources";
    rev = "d762461de45e00c73a514408988345691f727632";
    sha256 = "08msc3mgf1qzz6j82gi10fin12iwl2zh5annfgbp6nkig63j6fcx";
  } + "/xresources/base16-spacemacs-256.Xresources");
  home.packages = with pkgs; [
    nativefier
    python38Packages.youtube-dl-light
    nixfmt
    ffmpeg-full
    discord
    antibody
    gopass
    qutebrowser
    todoist-electron
    spotify
    teams
    nodejs
    kotatogram-desktop
    hexchat
    mu
    calibre
    tdesktop
    cargo
    rustc
    bat
    exa
    toggldesktop
    neovim
    sxiv
    deluge
    kdenlive
    transmission-gtk
  ];
  programs.zathura.enable = true;

  services.random-background = {
    enable = false;
    imageDirectory = "%h/images/wallpapers";
  };
  home.keyboard = {
    layout = "us";
    variant = "dvp";
    options = [ "ctrl:nocaps" "compose:altgr" ];
  };
  programs.beets = {
    enable = false;
    settings = { "directory" = "/data/music"; };
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
    plugins = with pkgs.tmuxPlugins; [ sensible yank ];
  };

}
