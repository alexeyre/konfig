{ config, lib, pkgs, ... }:
with lib;
let isMac = lib.hasSuffix "darwin" pkgs.system;
in {
  options.alex.setapp.enable = mkOption {
    type = types.bool;
    default = true;
    description = "Whether to use setapp";
  };
  config = mkIf config.alex.setapp.enable { alex.brew.casks = [ "setapp" ]; };

}
