{ config, lib, pkgs, ... }:
with lib; {
  options.alex.setapp.enable = mkOption {
    type = types.bool;
    default = true;
    description = "Whether to use setapp";
  };
  config = mkIf (config.alex.setapp.enable && config.alex.is-mac) {
    alex.brew.casks = [ "setapp" ];
  };

}
