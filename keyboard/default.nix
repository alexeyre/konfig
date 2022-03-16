{ config, lib, pkgs, ... }:
with lib; {
  home-manager.users.alex = {
    programs.brew.casks = [ "karabiner-elements" "programmer-dvorak" ];
    programs.brew.formulae = [ "yqrashawn/goku/goku" ];
    programs.brew.taps = [ "yqrashawn/goku" ];
    home.file.gokuConfig = {
      source = ./karabiner.edn;
      target = ".config/karabiner.edn";
      onChange = ''
        goku 2> /dev/null || true
      '';
    };
  };
}
