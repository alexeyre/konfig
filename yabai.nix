{ config, lib, pkgs, ... }: {
  home-manager.users.alex = { ... }: {
    home.file.yabairc = {
      text = ''
yabai -m config layout				 bsp
yabai -m config top_padding    20
yabai -m config bottom_padding 20
yabai -m config left_padding   20
yabai -m config right_padding  20
yabai -m config window_gap     20
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
    };
    programs.brew.formulae = [ "koekeishiya/formulae/yabai" ];

  };
}
