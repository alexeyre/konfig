{ config, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.iterm2 ];
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;
  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "right";
  system.defaults.dock.minimize-to-application = true;
  system.defaults.dock.show-recents = false;
  system.defaults.dock.tilesize = 32;
  time.timeZone = "Europe/London";
  nix.package = pkgs.nix;
  environment.darwinConfig = "\$HOME/.config/nixpkgs/configuration.nix";
  programs.zsh.enable = true; # default shell on catalina
  system.stateVersion = 4;
}
