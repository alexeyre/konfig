{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ../../general ./alfred ./keyboard ./vimari ];

  system.defaults.NSGlobalDomain.InitialKeyRepeat = 10;
  system.defaults.NSGlobalDomain.KeyRepeat = 1;
  system.defaults.dock.autohide = true;
  system.defaults.dock.orientation = "right";
  system.defaults.dock.minimize-to-application = true;
  system.defaults.dock.show-recents = false;
  system.defaults.dock.tilesize = 32;
  system.defaults.dock.autohide-delay = "0.0";
  system.defaults.dock.autohide-time-modifier = "0.4";

  time.timeZone = "Europe/London";

  environment.darwinConfig = "$HOME/.config/nix/configuration.nix";

  environment.pathsToLink = [ "/share/zsh" "/opt/homebrew/share/zsh" ];


  # ugly hack
  users.users.alex.home = "/Users/alex";

  environment.systemPath = let
    homebrew_directory =
      config.home-manager.users."${config.main-user}".programs.brew.directory;
  in [ "${homebrew_directory}/bin" "/opt/local/bin" ];

  home-manager.users."${config.main-user}" = { ... }: with config.home-manager.users."${config.main-user}"; {
    imports = [ ./brew ./iina.nix ];

    home.packages = [ (pkgs.writeScriptBin "activate_conda" ''
      source ~/.local/share/miniconda3/bin/activate
      '') ];

    programs.brew.enable = true;

    programs.brew.formulae = [ "gh" ]
      ++ optional programs.fish.enable "fish" # install fish on macOS
      ++ optional programs.tmux.enable "tmux";

    programs.iina.enable = true;

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
      "radio-silence"
      "spotify"
      "monero-wallet"
      "zotero"
      "rectangle"
      "discord"
      "steam"
    ];
    programs.brew.taps = [ "homebrew/bundle" "homebrew/core" "homebrew/cask" ];
  };
}
