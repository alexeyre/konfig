{ config, lib, pkgs, ... }:
with lib;
{
  options.alex.fasd.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure fasd";
  };
  config = lib.mkIf config.alex.fasd.enable {
    programs.zsh.initExtra = lib.mkIf config.alex.shell.enable ''
      eval "$(fasd --init auto)"
    '';
    home.packages = [ pkgs.fasd ];
  };
}
