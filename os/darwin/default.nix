{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ../../general ./alfred ./keyboard ./iterm ./vimari ];

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

  # ugly hack
  users.users.alex.home = "/Users/alex";

  home-manager.users."${config.main-user}" = { ... }: {
    imports = [ ./brew ];

    # install fish on macOS
    programs.brew.formulae = [ "gh" ]
      ++ optional config.programs.fish.enable "fish"; # install fish on macOS

    programs.fish.shellInit = let
      homebrew_directory =
        config.home-manager.users."${config.main-user}".programs.brew.directory;
    in ''
      set -U fish_user_paths ${homebrew_directory} $fish_user_paths
    '';

    # silence macOS MOTD message
    home.file.hushenv = {
      text = "thisisempty";
      target = ".hushlogin";
    };

    programs.brew.casks = [
      "homebrew/cask/programmer-dvorak"
      "alfred"
      "anki"
      "telegram"
      "veracrypt"
      "macfuse"
      "iina"
      "radio-silence"
      "spotify"

      "battle-net"
    ];
    programs.brew.taps = [ "homebrew/bundle" "homebrew/core" "homebrew/cask" ];
  };
}
