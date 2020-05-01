{ pkgs, ... }:
{
  xsession.windowManager.bspwm = {
    enable = true;
    monitors = { 
	    "eDP-1" = [ "browser" "emacs" "music" "msg" "terminal" ]; 
	    "eDP-1-1" = [ "browser" "emacs" "music" "msg" "terminal" ]; 
	    "eDP-3.2" = [ "browser" "emacs" "music" "msg" "terminal" ]; 
    };
    rules = {
      "KotatogramDesktop" = { desktop = "msg"; follow = true; };
      "Hexchat" = { desktop = "msg"; follow = true; };
      "Microsoft Teams - Preview" = { desktop = "msg"; follow = true; };
      "Emacs" = {state = "tiled"; desktop = "emacs"; follow = true; };
      "Mpv".state = "floating";
      "qutebrowser" = { desktop = "browser"; follow = true; };
      "Spotify" = { desktop = "music"; follow = true; };
      "spotify" = { desktop = "music"; follow = true; };
    };
    settings = {
      top_padding = 22;
      focus_follows_pointer = true;
      window_gap = 4;
      border_width = 1;
      honor_size_hints = true;
      focused_border_color = "#fc20bb";
      pointer_follows_monitor = true;
      pointer_follows_focus = true;
      remove_disabled_monitors = true;
      remove_unplugged_monitors = true;
    };
    startupPrograms = let 
      bspc = "${pkgs.bspwm}/bin/bspc";
      xrandr = "${pkgs.xorg.xrandr}/bin/xrandr";
      systemctl = "${pkgs.systemd}/bin/systemctl";
    in [ 
"${xrandr} --output DP-3.2 --mode 1920x1080 --rate 144 --same-as eDP-1-1 && ${xrandr} --output DP-3.1 --mode 1920x1080 --rotate right --left-of DP-3.2 && ${systemctl} --user restart polybar.service random-background.service"
    ];
  };

}
