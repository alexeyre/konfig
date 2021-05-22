{ config, lib, pkgs, ... }:
with lib; {
  main-user.home.activation.linkBartender = let
    bartender-type = if builtins.pathExists "/Applications/Setapp.app" then
      "Bartender-setapp"
    else
      "Bartender";
  in config.lib.dag.entryAfter [ "writeBoundry" ] ''
    ${pkgs.unison}/bin/unison $HOME/.local/dot/os/darwin/bartender/com.surteesstudios.Bartender.plist $HOME/Library/Preferences/com.surteesstudios.${bartender-type}.plist

  '';
  main-user.brew.casks = [ "bartender" ];
}
