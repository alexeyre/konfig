{ config, pkgs, lib, fetchgit, ... }:
let cfg = config.programs.konsole;
in {
  options = {
    programs.konsole = {
      enable = lib.mkEnableOption "KDE Plasma Console Emulator";
    };
  };
  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.konsole ];
    home.sessionVariables = { TERMINAL = "${pkgs.konsole}/bin/konsole"; };
  };
}
