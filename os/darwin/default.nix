{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ../../general ./alfred ./keyboard ./bartender ./iterm ./vimari ];

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
    programs.tmux.shell = let
      zsh_arm = (pkgs.writeScriptBin "zsh_arm" ''
        #!${pkgs.stdenv.shell}
          arch -arm64e /bin/zsh
      '');
    in "${zsh_arm}/bin/zsh_arm";
    programs.brew.formulae = [ "gh" ];
    programs.brew.casks = [
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

      "battle-net"
    ];
    programs.brew.taps = [ "homebrew/bundle" "homebrew/core" "homebrew/cask" ];
    programs.zsh.dirHashes = {
      artifacts = "$HOME/.CMVolumes/University/Artifacts";
    };
  };
}
