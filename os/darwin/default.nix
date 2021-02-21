{ config, pkgs, ... }:

{
  imports = [ ../../main.nix ];
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;
  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "right";
  system.defaults.dock.minimize-to-application = true;
  system.defaults.dock.show-recents = false;
  system.defaults.dock.tilesize = 32;
  time.timeZone = "Europe/London";
  environment.darwinConfig = "$HOME/.config/nixpkgs/configuration.nix";
  users.users.alex.home = pkgs.lib.mkForce "/Users/alex";

  home-manager.users.alex = (import ./home.nix);

  programs.zsh.enable = true; # default shell on catalina
  system.stateVersion = 4;

  home-manager.useGlobalPkgs = true;

  # home-manager.useUserPackages = true;
}
