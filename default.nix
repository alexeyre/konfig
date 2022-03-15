{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ../../general ./alfred ./keyboard ./vimari ];

  environment.darwinConfig = "$HOME/.config/nix/configuration.nix";

  # ugly hack
  # users.users.alex.home = "/Users/alex";

  environment.systemPath = let
    homebrew_directory =
      config.home-manager.users."${config.main-user}".programs.brew.directory;
  in [] ++ optional (config.home.programs.brew.enable) [ "${homebrew_directory}/bin" ];

  config.home = { ... }: {
      imports = [ ./brew ];

      programs.brew.enable = true;

      programs.brew.casks = [
        "homebrew/cask/programmer-dvorak"
      ];
    };
}
