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

  main-user = { ... }: {
    imports = [ ./modules ];
    programs.tmux.shell = let
      zsh_arm = (pkgs.writeScriptBin "zsh_arm" ''
        #!${pkgs.stdenv.shell}
          arch -arm64e /bin/zsh
      '');
    in mkIf config.alex.is-mac "${zsh_arm}/bin/zsh_arm";
    alex.brew.enable = true;
    alex.tmux.enable = true;
    alex.brew.casks = [ "battle-net" ];
    alex.brew.formulae = [ "gh" ];
    alex.keyboard.karabiner.enable = true;
    alex.bartender.enable = true;
    alex.iterm.enable = true;
    alex.brew.casks = [
      "homebrew/cask/programmer-dvorak"
      "alfred"
      "anki"
      "telegram"
      "veracrypt"
      "macfuse"
      "iina"
      "radio-silence"
      "notion"
      "toggl-track"
      "spotify"
    ];
    alex.setapp.enable = true;
    alex.vimari.enable = true;
    alex.brew.taps = [ "homebrew/bundle" "homebrew/core" "homebrew/cask" ];
    programs.zsh.dirHashes = {
      artifacts = "$HOME/.CMVolumes/University/Artifacts";
    };
  };
}
