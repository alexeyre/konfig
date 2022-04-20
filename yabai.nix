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
                yabai -m config mouse_modifier fn

                # rules
                yabai -m rule --add label=obsidian app="^Obsidian$" space=3
                yabai -m rule --add app="^Accessibility Inspector$" manage=off
        yabai -m rule --add app="^Activity Monitor$" manage=off
        yabai -m rule --add app="^AlDente$" manage=off
        yabai -m rule --add app="^coconutBattery$" manage=off
        yabai -m rule --add app="^Disk Utility$" manage=off
        yabai -m rule --add app="^Digital Colour Meter$" manage=off
        yabai -m rule --add app="^FaceTime$" manage=off
        yabai -m rule --add app="^Finder$" manage=off
        yabai -m rule --add app="^Firefox$" title="^Opening " manage=off
        yabai -m rule --add app="^Font Book$" manage=off
        yabai -m rule --add app="^Installer$" manage=off
        yabai -m rule --add app="^Keka$" manage=off
        yabai -m rule --add app="^KeyboardCleanTool$" manage=off
        yabai -m rule --add app="^Keychain Access$" manage=off
        yabai -m rule --add app="^MAMP$" manage=off
        yabai -m rule --add app="^Mail$" manage=off
        yabai -m rule --add app="^Photos$" manage=off
        yabai -m rule --add app="^Postgres$" manage=off
        yabai -m rule --add app="^Slack$" manage=off
        yabai -m rule --add app="^Stickies$" manage=off
        yabai -m rule --add app="^System Information$" manage=off
        yabai -m rule --add app="^System Preferences$" manage=off
        yabai -m rule --add app="^Transmission$" manage=off
        yabai -m rule --add app="^Unclutter$" manage=off
                      '';
      target = ".config/yabai/yabairc";
      executable = true;
    };

    home.file.skhd = {
      text = ''
        alt - v : yabai -m space --focus next
        alt - w : yabai -m space --focus prev

        alt - space : yabai -m window

        # terminal
        alt - return : /Applications/kitty.app/Contents/MacOS/kitty --single-instance -d ~ &> /dev/null

        # movement
        shift + alt - d : yabai -m window --warp west
        shift + alt - s : yabai -m window --warp east
        shift + alt - h : yabai -m window --warp south
        shift + alt - t : yabai -m window --warp north

        shift + cmd - d : yabai -m window --swap west
        shift + cmd - s : yabai -m window --swap east
        shift + cmd - h : yabai -m window --swap south
        shift + cmd - t : yabai -m window --swap north

        # focus
        alt - d : yabai -m window --focus west
        alt - h : yabai -m window --focus south
        alt - t : yabai -m window --focus north
        alt - n : yabai -m window --focus east
              '';
      target = ".config/skhd/skhdrc";
    };
    programs.brew.formulae = [ "yabai" "skhd" ];
    programs.brew.taps = [ "koekeishiya/formulae" ];

  };
}
