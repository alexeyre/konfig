{ config, lib, pkgs, ... }:
with lib;
let cfg = config.programs.rofi;
in {
  options.programs.rofi.replaceDmenu = mkOption {
    default = false;
    type = types.bool;
    description = ''
      Replace dmenu in the system path?
    '';
  };
  config = let
    rofiDmenuWrapper = pkgs.writeScriptBin "dmenu" ''
      #!${pkgs.stdenv.shell}
            exec ${pkgs.rofi}/bin/rofi -dmenu "\$@"

    '';
  in mkIf cfg.replaceDmenu { home.packages = [ rofiDmenuWrapper ]; };
}
