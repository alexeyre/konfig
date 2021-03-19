{ config, lib, pkgs, ... }:
with lib; {
  options.alex.keyboardLayout = mkOption {
    type = types.string;
    default = "dvp";
    description = "Keyboard layout to use for module configuration";
  };
}
