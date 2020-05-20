{ pkgs, ... }: {
  imports = [ ./module.nix ];
  xsession.windowManager.bspwm = {
    enable = true;
    monitors = {
      "eDP1" = [ "browser" "emacs" "music" "msg" "terminal" ];
      "eDP-1" = [ "browser" "emacs" "music" "msg" "terminal" ];
      "eDP-1-1" = [ "browser" "emacs" "music" "msg" "terminal" ];
      "eDP-3.2" = [ "browser" "emacs" "music" "msg" "terminal" ];
    };
    rules = {
      "KotatogramDesktop" = {
        desktop = "msg";
        follow = true;
      };
      "Hexchat" = {
        desktop = "msg";
        follow = true;
      };
      "Microsoft Teams - Preview" = {
        desktop = "msg";
        follow = true;
      };
      "Emacs" = {
        state = "tiled";
        desktop = "emacs";
        follow = true;
      };
      "Mpv".state = "floating";
      "qutebrowser" = {
        desktop = "browser";
        follow = true;
      };
      "Spotify" = {
        desktop = "music";
        follow = true;
      };
      "spotify" = {
        desktop = "music";
        follow = true;
      };
      "plasmashell" = {
        state = "floating";
        border = false;
        "layer" = "normal";
      };
      "krunner" = {
        state = "floating";
        "border" = false;
      };
      "discord" = { state = "floating"; };
    };
    settings = {
      top_padding = 26;
      focus_follows_pointer = true;
      window_gap = 4;
      border_width = 1;
      honor_size_hints = true;
      focused_border_color = "#fc20bb";
      pointer_follows_monitor = true;
      pointer_follows_focus = false;
      remove_disabled_monitors = true;
      remove_unplugged_monitors = true;
    };
    startupPrograms = let
      bspc = "${pkgs.bspwm}/bin/bspc";
      xrandr = "${pkgs.xorg.xrandr}/bin/xrandr";
      lxqt = "${pkgs.lxqt.lxqt-session}/bin/startlxqt";
      feh = "${pkgs.feh}/bin/feh";
      wallpaper = builtins.fetchurl
        "https://raw.githubusercontent.com/alex-eyre/-/master/media/0ttvn2u117g41.png";
    in [
      "${lxqt}"
      "${xrandr} --output DP-3.2 --mode 1920x1080 --rate 144 --same-as eDP-1-1 && ${xrandr} --output DP-3.1 --mode 1920x1080 --rotate right --left-of DP-3.2"
      "${feh} --bg-tile ${wallpaper}"
    ];
  };

}
