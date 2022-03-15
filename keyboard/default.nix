{ config, lib, pkgs, ... }:
with lib; {
  home-manager.users."${config.main-user}" = {
    programs.brew.casks = [ "karabiner-elements" ];
    programs.brew.formulae = [ "yqrashawn/goku/goku" ];
    home.file.gokuConfig = {
      source = ./karabiner.edn;
      target = ".config/karabiner.edn";
      onChange = ''
        goku 2> /dev/null || true
      '';
    };
  };
}
