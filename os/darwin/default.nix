{ config, pkgs, ... }: {
  imports = [ ../../main.nix ];
  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;
  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "right";
  system.defaults.dock.minimize-to-application = true;
  system.defaults.dock.show-recents = false;
  system.defaults.dock.tilesize = 32;
  time.timeZone = "Europe/London";
  environment.darwinConfig = "$HOME/.config/nix/configuration.nix";
  users.users.alex.home = "/Users/alex";
  environment.pathsToLink = [ "/share/zsh" "/opt/homebrew/share/zsh" ];
  home-manager.useGlobalPkgs = true;
  home-manager.backupFileExtension = ".backup";
  # touch ID in tmux
  home-manager.users.alex.alex.brew.taps = [ "fabianishere/personal" ];
  home-manager.users.alex.alex.brew.formulae = [ "pam_reattach" ];

}
