{ config, lib, pkgs, ...}:
with lib;
{
  options.alex.toggl.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to install tools for the toggl time-tracking service";
  };

  config = mkIf config.alex.toggl.enable {
  };
}
