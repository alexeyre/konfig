{ config, lib, pkgs, ... }:
with lib; {
    home-manager.users."${config.main-user}" = {
    programs.brew.casks = [ "karabiner-elements" ];

    home.file.karabiner =
      mkIf (config.system.keyboard.layout == "dvp") {
        source = ./karabiner.json;
        target = ".config/karabiner/karabiner.json";
      };
      };
}
