{ pkgs, config, ... }: {
  services.sxhkd = {
    enable = true;
    keybindings = let
      bspc = "${pkgs.bspwm}/bin/bspc";
      launcher =
        "rofi -combi-modi window,ssh,drun -modi combi,run -icon-theme Arc-Dark -show-icons -matching=fuzzy -show combi";
      term = "${config.home.sessionVariables.TERMINAL}";
      dmenu = "${pkgs.rofi}/bin/rofi -dmenu";
      sed = "${pkgs.gnused}/bin/sed";
      pass = "${pkgs.gopass}/bin/gopass";
      pactl = "${pkgs.pulseaudio}/bin/pactl";
      lock = "${pkgs.xautolock}/bin/xautolock -locknow";
    in {
      "super + shift + l" = "${lock}";

      "XF86Audio{Raise,Lower}Volume" = "${pactl} set-sink-volume 0 {+,-}5%";
      "XF86AudioMute" = "${pactl} set-sink-mute 0 toggle";
      "XF86AudioMicMute" = "${pactl} set-source-mute 1 toggle";

      "super + shift + d" =
        "systemctl --user restart random-background.service";
      "super + shift + control + r" = "pkill -9 bspwm";
      "super + ctrl + Return" = "${term}";
      "super + Return" = ''
        ${bspc} desktop -f terminal && if ${bspc} query -D -d .!occupied --names | grep -q "terminal"; then exec ${term}; fi'';
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
      "super + bracketright" = "${bspc} desktop -f ^10.local";

      "super + shift + j" = "${bspc} desktop -f prev.local";
      "super + shift + k" = "${bspc} desktop -f next.local";
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

      "super + semicolon ; p" = "${pass} -c $(${pass} ls --flat | dmenu)";

      "super + o" =
        "desired=$(${bspc} query -D --names | ${dmenu}) ; ${bspc} desktop -f $desired || ${bspc} monitor -a $desired && ${bspc} desktop -f $desired";
      "super + d" = "${bspc} desktop $(${bspc} query -D --names | ${dmenu}) -r";
      "super + shift + o" =
        "desired=$(${bspc} query -D --names | ${dmenu}) ; ${bspc} node -d $desired || ${bspc} monitor -a $desired && ${bspc} node -d $desired";
      "super + shift + p" =
        "command=$(${pkgs.dmenu}/bin/dmenu_path | ${dmenu}) ; ${bspc} desktop -f $command || ${bspc} monitor -a $command && ${bspc} desktop -f $command && exec $command &";
      "super + alt + ctrl + p" = ''
        for x in $(${bspc} query -D -d .\!occupied --names | ${sed} '/browser/d;/terminal/d;/emacs/d;/music/d;/msg/d;'); do ${bspc} desktop "$x" -r; done'';
      "super + Tab" = "${bspc} desktop -f last.local";
      "alt + Tab" = "${bspc} node -f last.local";
      "super + shift + comma" = "${bspc} desktop -m next --follow";
      "super + comma" = "${bspc} monitor -f next";
      "super + r" = ''
        ${bspc} desktop -n "$(${bspc} query -D -d .focused --names | dmenu)"'';

      "super + h" = "${bspc} node -f west";
      "super + j" = "${bspc} node -f south";
      "super + k" = "${bspc} node -f north";
      "super + l" = "${bspc} node -f east";

      "super + shift + Return" = "${bspc} node -s next.local.!floating";

      "super + shift + f" = "${bspc} node -t \\~fullscreen";

      "super + s" =
        "${bspc} node -g sticky && ${bspc} node -g locked && ${bspc} node -g private";

      "super + q ; b" = "${bspc} desktop -f browser";
      "super + q ; e" = "${bspc} desktop -f emacs";
      "super + q ; m" = "${bspc} desktop -f msg";
      "super + q ; t" = "${bspc} desktop -f terminal";

      "super + a ; e" = "emacsclient -cuqn $HOME/notes/home.org";
    };
  };
}
