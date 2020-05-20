{ pkgs, ... }: {
  services.polybar = {
    enable = true;
    package = pkgs.polybarFull;
    config = {
      "global/wm" = { margin_top = 22; };
      "bar/top" = {
        width = "100%";
        modules-right = "pulseaudio eth wlan battery";
        modules-left = "";
        modules-center = "time";
        height = "26";
        radius = 0;
        font-0 = "fixed";
        font-1 = "FiraCode Nerd Font:pixelsize=10;-1";
        font-2 = "Noto Color Emoji:scale=10:style=Regular;2";
        module-margin-left = 0;
        module-margin-right = 1;
        overline-size = 1;
        overline-color = "\${colors.iopink}";
        line-size = 1;
        line-color = "\${colors.red}";
        wm-restack = "bspwm";
        override-redirect = true;
        enable-ipc = true;
        # monitor = "eDP-1";
      };
      "bar/bottom" = {
        witdh = "100%";
        height = "26";
        modules-left = "bspwm";
        bottom = true;
      };
      "module/time" = {
        type = "internal/date";
        interval = 60;
        time = "%d-%M-%Y %H:%M";
        label = "%time%";
        label-foreground = "\${colors.green}";
      };
      "module/bspwm" = {
        type = "internal/bspwm";
        pin-workspaces = false;
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
      "module/wlan" = {
        type = "internal/network";
        interface = "wlp2s0";
        label-connected = "%essid%";
        label-disconnected = "";
      };
      "module/eth" = {
        type = "internal/network";
        interface = "enp0s31f6";
        label-connected = "%local_ip%";
        label-disconnected = "";
      };
      "module/pulseaudio" = {
        type = "internal/pulseaudio";
        interval = "5";
        label-foreground = "\${colors.urlblue}";
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
    script = let awk = "${pkgs.gawk}/bin/awk";
    in "for i in $(polybar -m | ${awk} -F: '{print $1}'); do MONITOR=$i polybar top & MONITOR=$i polybar bottom & done";
  };
}
