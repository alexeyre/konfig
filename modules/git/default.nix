{ config, lib, pkgs, ... }:
with lib;
let isMac = lib.hasSuffix "darwin" pkgs.system;
in {
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
    programs.git.package = mkIf isMac pkgs.hello;
    alex.brew.formulae = mkIf isMac [ "git" ];
  };
}
