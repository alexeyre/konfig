{ config, lib, pkgs, ... }:
with lib;
{
  options.alex.bartender.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to install and configure bartender";
  };
  config = mkIf (config.alex.bartender.enable && config.alex.is-mac) {
    home.activation.linkBartender = let bartender-type = if config.alex.setapp.enable then "Bartender-setapp" else "Bartender"; in config.lib.dag.entryAfter [ "writeBoundry" ] ''
      ln -sf ${config.alex.dots}/os/darwin/com.surteesstudios.Bartender.plist $HOME/Library/Preferences/com.surteesstudios.${bartender-type}.plist
    '';
    alex.brew.casks = mkIf (!config.alex.setapp.enable) [ "bartender" ];
  };
}
