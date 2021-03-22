{ config, lib, pkgs, ... }:
with lib;
{
  options.alex.git.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to install and configure git";
  };
  config = mkIf config.alex.git.enable {
    programs.git = {
      enable = true;
      userName = "Alex Eyre";
      userEmail = "alexeeyre@gmail.com";
    };
    programs.git.package = mkIf config.alex.is-mac pkgs.hello;
    # alex.brew.formulae = mkIf config.alex.is-mac [ "git" ];
  };
}
