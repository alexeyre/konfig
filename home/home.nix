{ config, lib, pkgs, ... }:
{
  imports = [
    ../overlays.nix
    ./qute.nix
    ./emacs.nix
    ./term/term.nix
  ];
  
  programs.git = {
    enable = true;
    userName = "Alex Eyre";
    userEmail = "alex@turm.pw";
  };
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
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    defaultKeymap = "viins";
    initExtra = ''
      source <(antibody init)
      MODE_CURSOR_VICMD="#fc20bb block"
      MODE_CURSOR_VIINS="#fc20bb blinking bar"
      MODE_CURSOR_SEARCH="#fc20bb steady underline"
      antibody bundle softmoth/zsh-vim-mode
    '';
    dotDir = ".config/zsh";
    sessionVariables = { ZSH_TMUX_AUTOSTART="true"; };
    oh-my-zsh = {
      enable = true;
      theme = "agnoster";
      plugins = [ "git" "tmux" "vi-mode" ];
    };
  };
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
  home.packages = with pkgs; [
    light
    discord-canary
    kotatogram-desktop
    antibody
    gopass
    qutebrowser
    zoom-us
    dmenu
    vim
    spotify
    teams
    nodejs
    tor-browser-bundle-bin
    kotatogram-desktop
    hexchat
    anki

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
    zathura
  ];
  services.random-background = {
    enable = true;
    imageDirectory = "%h/images/wallpapers";
  };
  home.stateVersion = "19.09";
  services.keybase.enable = true;
  home.keyboard = {
    layout = "us";
    variant = "dvp";
    options = ["ctrl:nocaps"];
  };
  programs.mpv = {
    enable = true;
    profiles = {
      pip = {
        ontop = true;
        autofit = "384x216";
        geometry = "99%:2%";
      };
    };
    config = {
      profile = "gpu-hq,pip";
      scale= "ewa_lanczossharp";
      cscale = "ewa_lanczossharp";
      video-sync = "display-resample";
      ytdl-format = "bestvideo+bestaudio";
      tscale = "oversample";
    };
    bindings = {
      "Ctrl+p" = "run bspc -t floating ; run bspc -g sticky=on ; apply-profile pip";
    };
  };

  programs.newsboat = {
    enable = true;
    extraConfig = ''
      macro y set browser "mpv --profile=pip %u"; open-in-browser ; set browser "elinks %u"
      unbind-key h
      unbind-key j
      unbind-key k
      unbind-key l

      bind-key h quit
      bind-key j down
      bind-key k up
      bind-key l open
      '';
    queries.unread = "unread = \"yes\"";
    urls = [
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"; } # lukesmithxyz
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCwgKmJM4ZJQRJ-U5NjvR2dg"; } # commaai archive
      # { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCGSGPehp0RWfca-kENgBJ9Q"; } # jreg
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC7SeFWZYFmsm1tqWxfuOTPQ"; } # some scottish man
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCKuDLsO0Wwef53qdHPjbU2Q"; } # William Lin
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw"; } # 3blue1brown
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC52kszkc08-acFOuogFl5jw"; } # tibees
    ];
  };
  xsession.enable = true;
  xsession.windowManager.bspwm = {
    enable = true;
    monitors = { };
    rules = {
      "KotatogramDesktop".desktop = "msg";
      "Hexchat".desktop = "msg";
      "Microsoft Teams - Preview".desktop = "msg";
      "Emacs".state = "tiled";
      "mpv".state = "floating";
      "qutebrowser".desktop = "browser";
      "Qutebrowser".desktop = "browser";
      "spotify".desktop = "music";
      "Spotify".desktop = "music";
    };
    settings = {
      top_padding = 22;
      focus_follows_pointer = true;
      window_gap = 14;
      border_width = 0;
    };
    startupPrograms = let 
      hexchat = "${pkgs.hexchat}/bin/hexchat";
      tgram = "${pkgs.kotatogram-desktop}/bin/kotatogram-desktop";
      teams = "${pkgs.teams}/bin/teams";
      bspc = "${pkgs.bspwm}/bin/bspc";
    in [ 
      "systemctl --user restart polybar.service"
      "${tgram}"
      "${hexchat}"
      "${teams}"
    ];
  };

  services.redshift = {
    enable = true;
    latitude = "52.0";
    longitude = "0.0";
  };
  services.polybar = {
    enable = true;
    config = {
      "global/wm" = {
        margin_top = 22;
      };
      "bar/main" = {
        width = "100%";
        modules-right = "battery time";
        modules-left = "bspwm";
        height = "22";
        radius = 0;
        font-0 = "FiraCode Nerd Font:size=12;1";
        font-1 = "Noto Color Emoji:scale=10:style=Regular;2";
        module-margin-left = 0;
        module-margin-right = 1;
        overline-size = 1;
        overline-color = "\${colors.iopink}";
        line-size = 1;
        line-color = "\${colors.red}";
        wm-restack = "bspwm";
        override-redirect = true;
      };
      "module/time" = {
        type = "internal/date";
        interval = 60;
        time = "%H:%M";
        label = "%time%";
        label-foreground = "\${colors.green}";
      };
      "module/bspwm" = {
        type = "internal/bspwm";
        pin-workspaces = true;
        inline-mode = true;
        enable-click = true;
        fuzzy-match = true;
        label-separator = " |";
        label-focused-foreground = "\${colors.iopink}"; 
        label-empty-foreground = "\${colors.grey}";
        format = "<label-monitor> :: <label-state> ";
      };
      "module/battery" = {
        type = "internal/battery";
        battery = "BAT0";
        adapter = "AC";

        full-at = 99;
        label-charging-foreground = "\${colors.iolime}";
        label-discharging-foreground = "\${colors.red}";

        label-charging = "%percentage%%";
        label-discharging = "%percentage%%";
        format-charging = "<label-charging> <animation-charging>";
        format-discharging = "<label-discharging> <ramp-capacity>";

        animation-charging-0 = "";
        animation-charging-1 = "";
        animation-charging-2 = "";
        animation-charging-3 = "";
        animation-charging-4 = "";
        animation-charging-5 = "";
        animation-charging-6 = "";
        animation-charging-7 = "";
        animation-charging-8 = "";
        animation-charging-9 = "";
        animation-charging-framerate = 250;

        ramp-capacity-0 = "";
        ramp-capacity-1 = "";
        ramp-capacity-2 = "";
        ramp-capacity-3 = "";
        ramp-capacity-4 = "";
        ramp-capacity-5 = "";
        ramp-capacity-6 = "";
        ramp-capacity-7 = "";
        ramp-capacity-8 = "";
        ramp-capacity-9 = "";
        ramp-capacity-10 = "";
      };
    };
    extraConfig = ''
      [colors]
      bg        = #1b1d1e
      bg-alt    = #262829
      base0     = #1b1d1e
      base1     = #202020
      base2     = #303030
      base3     = #303030
      base4     = #505050
      base5     = #505050
      base6     = #808080
      base7     = #808080
      base8     = #DFDFDF
      fg        = #dddddd
      fg-alt    = #5B6268

      grey      = #505050
      red       = #d02b61
      orange    = #da8548
      green     = #60aa00
      teal      = #4db5bd
      yellow    = #d08928
      blue      = #6c9ef8
      dark-blue = #6688aa
      magenta   = #b77fdb
      violet    = #a9a1e1
      cyan      = #00aa80
      dark-cyan = #5699AF
      urlblue   = #57aadd
      iolime    = #bbfc20
      iopurple  = #bb20fc
      iocyan    = #20bbfc
      iopink    = #fc20bb
      ioteal    = #20fcbb
    '';
    script = "polybar main &";
  };
  programs.tmux = {
    enable = true;
    keyMode = "vi";
    shortcut = "a";
    plugins = with pkgs; [ tmuxPlugins.sensible tmuxPlugins.yank ];
  };
  services.sxhkd = {
    enable = true;
    keybindings = let
      bspc = "${pkgs.bspwm}/bin/bspc";
      launcher = "${pkgs.dmenu}/bin/dmenu_run";
      term = "${config.home.sessionVariables.TERMINAL}";
      dmenu = "${pkgs.dmenu}/bin/dmenu";
      sed = "${pkgs.gnused}/bin/sed";
    in
      {
        "super + shift + d" = "systemctl --user restart random-background.service";
        "super + shift + r" = "systemctl --user restart sxhkd.service";
        "super + Return" = "${term}";
        "super + p" = "${launcher}";
        "super + shift + c" = "${bspc} node -c";
        "super + space" = "${bspc} node -t \\~floating";

        "super + ampersand" = "${bspc} desktop -f ^1.local";
        "super + bracketleft" = "${bspc} desktop -f ^2.local";
        "super + braceleft" = "${bspc} desktop -f ^3.local";
        "super + braceright" = "${bspc} desktop -f ^4.local";
        "super + parenleft" = "${bspc} desktop -f ^5.local";
        "super + equal" = "${bspc} desktop -f ^6.local";
        "super + asterisk" = "${bspc} desktop -f ^7.local";
        "super + parenright" = "${bspc} desktop -f ^8.local";
        "super + plus" = "${bspc} desktop -f ^9.local";
        "super + bracketright" = "${bspc} desktop -f ^10.locl";

        "super + shift + ampersand" = "${bspc} node -d ^1.local";
        "super + shift + bracketleft" = "${bspc} node -d ^2.local";
        "super + shift + braceleft" = "${bspc} node -d ^3.local";
        "super + shift + braceright" = "${bspc} node -d ^4.local";
        "super + shift + parenleft" = "${bspc} node -d ^5.local";
        "super + shift + equal" = "${bspc} node -d ^6.local";
        "super + shift + asterisk" = "${bspc} node -d ^7.local";
        "super + shift + parenright" = "${bspc} node -d ^8.local";
        "super + shift + plus" = "${bspc} node -d ^9.local";
        "super + shift + bracketright" = "${bspc} node -d ^10.local";


        "super + o" = "desired=\$(${bspc} query -D --names | ${dmenu}) ; ${bspc} desktop -f $desired || ${bspc} monitor -a $desired && ${bspc} desktop -f $desired";
        "super + d" = "${bspc} desktop \$(${bspc} query -D --names | ${dmenu}) -r"; 
        "super + shift + o" = "desired=\$(${bspc} query -D --names | ${dmenu}) ; ${bspc} node -d $desired || ${bspc} monitor -a $desired && ${bspc} node -d $desired";
        "super + shift + p" = "command=\$(dmenu_path | dmenu) ; bspc desktop -f $command || bspc monitor -a $command && bspc desktop -f $command && exec $command &";
        "super + alt + ctrl + p" = "for x in \$(${bspc} query -D -d .\\!occupied --names | ${sed} \'\/browser\/d\;\/terminal\/d\'); do ${bspc} desktop \"\$x\" -r; done";
        "super + Tab" = "${bspc} desktop -f last";
        "alt + Tab" = "${bspc} node -f last.local";
        "super + shift + comma" = "${bspc} desktop -m next --follow";
        "super + comma" = "${bspc} monitor -f next";
        "super + r" = "${bspc} desktop -n \"\$(${bspc} query -D -d \.focused --names | dmenu)\"";

        "super + shift + Return" = "${bspc} node -s next\.local";
        "super + shift + q" = "pkill -9 Xorg";
        
        "super + shift + f" = "${bspc} node -t \\~fullscreen";
        
        "super + s" = "${bspc} node -g sticky -g locked";
      };
	};

}
