{ config, lib, pkgs, ... }:
with lib; {
  options.alex.is-mac = mkOption {
    type = types.bool;
    default = lib.hasSuffix "darwin" pkgs.system;
    description = "Are we on a mac system?";
    readOnly = true;
  };
}
