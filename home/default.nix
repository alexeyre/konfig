{ config, lib, pkgs, ... }: {
  imports = [
    ./emacs
    ./term/term.nix
    ./bspwm
    # ./xmonad.nix
    ./email.nix
    ./git.nix
    ./mpv.nix
    ./newsboat.nix
    ./polybar.nix
    ./sxhkd.nix
    ./zsh.nix
    ./audacity.nix
    ./rofi
    ./qute
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
    enable = false;
    fade = true;
    fadeDelta = 3;
    vSync = true;
    extraOptions = ''
      corner-radius = 12.0;
      detect-rounded-corners = true;
      unredir-if-possible = false;
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
    opacityRule = [ "100:class_g = 'Mpv'" "100:class_g = 'mpv'" ];
    blur = true;
    experimentalBackends = true;
    activeOpacity = "0.92";
    inactiveOpacity = "0.92";
  };
  services.syncthing.enable = true;
  xresources.properties = {
    "*.font" = "FiraCode Nerd Font:pixelsize=18:antialias=true:autohint=true;";
  };
  xresources.extraConfig = builtins.readFile (pkgs.fetchFromGitHub {
    owner = "base16-templates";
    repo = "base16-xresources";
    rev = "d762461de45e00c73a514408988345691f727632";
    sha256 = "08msc3mgf1qzz6j82gi10fin12iwl2zh5annfgbp6nkig63j6fcx";
  } + "/xresources/base16-isotope-256.Xresources");
  home.packages = with pkgs; [
    acpi
    (pkgs.chromium.override { enableWideVine = true; })
    gitkraken
    gimp
    scrot
    nixfmt
    discord
    antibody
    gopass
    qutebrowser
    todoist-electron
    spotify
    mu
    # calibre # broken for some reason
    tdesktop
    neovim
    sxiv
    gnome3.nautilus
    winePackages.staging
    winePackages.fonts
    libreoffice-fresh
    steam
    fxkatana
  ];
  programs.zathura.enable = true;

  services.random-background = {
    enable = true;
    imageDirectory = "%h/images/wallpapers";
  };
  home.keyboard = {
    layout = "us";
    variant = "dvp";
    options = [ "ctrl:nocaps" "compose:ralt" ];
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
    shortcut = "a";
    keyMode = "vi";
    newSession = true;
    terminal = "tmux-256color";
    escapeTime = 0;
    customPaneNavigationAndResize = true;
    clock24 = true;
    baseIndex = 1;
    aggressiveResize = true;
  };
  programs.irssi = {
    enable = true;
    networks.freenode = {
      nick = "absoluutely";
      server = {
        address = "chat.freenode.net";
        port = 6697;
        autoConnect = true;
      };
      channels = { nixos.autoJoin = true; };
    };
  };

}
