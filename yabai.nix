{ config, lib, pkgs, ... }: {
  home-manager.users.alex = { ... }: {
    home.file.yabairc = {
      text = ''
        yabai -m signal --add event=dock_did_restart action="sudo yabai --load-sa"

        # Split ratio
        yabai -m config auto_balance on

        yabai -m config layout				 bsp
        yabai -m config top_padding    10
        yabai -m config bottom_padding 10
        yabai -m config left_padding   10
        yabai -m config right_padding  10
        yabai -m config window_gap     10
        yabai -m config window_topmost on
        yabai -m config window_shadow float
        yabai -m config focus_follows_mouse autofocus
        yabai -m config window_border on
        yabai -m config active_window_border_color 0xFF005577
        yabai -m config normal_window_border_color 0xFF444444
        yabai -m config window_border_width 4
        yabai -m config mouse_modifier cmd
              '';
      target = ".config/yabai/yabairc";
      onChange = ''
        chmod +x ~/.config/yabai/yabairc
        ~/.config/yabai/yabairc
      '';
    };
    programs.brew.formulae = [ "koekeishiya/formulae/yabai" ];
    programs.brew.taps = [ "koekeishiya/formulae" ];

  };
}
