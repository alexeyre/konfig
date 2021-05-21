{ config, lib, pkgs, ... }:
with lib; {
  options.alex.keyboard.layout = mkOption {
    type = types.str;
    default = "dvp";
    description = "Keyboard layout to use for module configuration";
  };
  options.alex.keyboard.karabiner.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Install karabiner-elements and configure it";
  };
  config = mkIf config.alex.keyboard.karabiner.enable {
    alex.brew.casks = [ "karabiner-elements" ];

    home.file.karabiner = {
      source = ./karabiner.json;
      target = ".config/karabiner/karabiner.json";
    };
  };
}
