{ config, pkgs, ... }: {
  imports = [ ../.. ];

  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;
  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "right";
  system.defaults.dock.minimize-to-application = true;
  system.defaults.dock.show-recents = false;
  system.defaults.dock.tilesize = 32;


  time.timeZone = "Europe/London";


  environment.darwinConfig = "$HOME/.config/nix/configuration.nix";


  environment.pathsToLink = [ "/share/zsh" "/opt/homebrew/share/zsh" ];


  home-manager.useGlobalPkgs = true;


  home-manager.backupFileExtension = ".backup";

  # ugly hack
  users.users.alex.home = "/Users/alex";
}
