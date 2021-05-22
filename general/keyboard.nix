{ config, lib, pkgs, ... }:
with lib; {
  options.system.keyboard.layout = mkOption {
    type = types.enum [ "dvp" "qwerty" ];
    default = "dvp";
    description = "Keyboard layout to use for module configuration";
  };
}
