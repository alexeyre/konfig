{ config, lib, pkgs }:
with lib;
{
  options.programs.iina.enable = mkEnableOption "Enable the IINA media player";
  config.programs.brew = mkIf config.options.programs.iina.enable {
    casks = [ "iina" ];
    formulae = [ "youtube-dl"];
  };
}
