{ config, lib, ... }:
with lib; {
  options.programs.iina.enable = mkEnableOption "Enable the IINA media player";
  config.programs.brew = mkIf config.programs.iina.enable {
    casks = [ "iina" ];
    formulae = [ "yt-dlp" ];
  };
}
