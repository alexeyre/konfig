{ config, lib, pkgs, ... }:
with lib; {
  options.alex.alfred.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to install and configure alfred";
  };
  config = mkIf (config.alex.alfred.enable && config.alex.is-mac) {
    alex.brew.casks = [ "alfred" ];
  };
}
