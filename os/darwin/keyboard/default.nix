{ config, lib, pkgs, ... }:
with lib; {
  config = {
    main-user.brew.casks = [ "karabiner-elements" ];

    main-user.home.file.karabiner =
      mkIf (config.system.keyboard.layout == "dvp") {
        source = ./karabiner.json;
        target = ".config/karabiner/karabiner.json";
      };
  };
}
