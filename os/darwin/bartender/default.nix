{ config, lib, pkgs, home-manager, ... }:
with lib; {
  home-manager.users."${config.main-user}" = {
    home.activation.linkBartender = let
      bartender-type = if builtins.pathExists "/Applications/Setapp.app" then
        "Bartender-setapp"
      else
        "Bartender";
    in home-manager.lib.hm.dag.entryAfter [ "writeBoundry" ] ''
      ${pkgs.unison}/bin/unison $HOME/.local/dot/os/darwin/bartender/com.surteesstudios.Bartender.plist $HOME/Library/Preferences/com.surteesstudios.${bartender-type}.plist

    '';
    programs.brew.casks = [ "bartender" ];
  };
}
