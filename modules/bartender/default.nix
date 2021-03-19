{ config, lib, pkgs, ... }:
with lib; {
  options.alex.bartender.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to install and configure bartender";
  };
  config = mkIf config.alex.bartender.enable {
    home.file.bartenderPreferences = {
      source = ./com.surteesstudios.Bartender-setapp.plist;
      target = "Library/Preferences/com.surteesstudios.Bartender-setapp.plist";
    };
    alex.brew.casks = [ "bartender" ];
  };
}
