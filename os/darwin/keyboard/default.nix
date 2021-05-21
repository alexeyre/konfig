{ config, lib, pkgs, ... }:
with lib; {
  options.system.keyboard.layout = mkOption {
    type = types.enum [ "dvp" "qwerty" ];
    default = "dvp";
    description = "Keyboard layout to use for module configuration";
  };
  config = {
    main-user.brew.casks = [ "karabiner-elements" ];

    main-user.home.file.karabiner =
      mkIf (config.system.keyboard.layout == "dvp") {
        source = ./karabiner.json;
        target = ".config/karabiner/karabiner.json";
      };
  };
}
