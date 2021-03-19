{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ./install.nix ];
  options.alex.brew.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Generate a ~/.Brewfile and sync it on generation change";
  };
  options.alex.brew.formulae = mkOption {
    type = types.listOf types.string;
    default = [];
    description = "The brew formula commands to pass to the home Brewfile";
  };
  options.alex.brew.casks = mkOption {
    type = types.listOf types.string;
    default = [];
    description = "The brew cask commands to pass to the home Brewfile";
  };
  config = {
    home.packages = [
      (pkgs.writeScriptBin "sync_brew" ''
        brew update &&
        ~/.local/share/brew/bin/brew bundle cleanup --global --zap --force -q &
        brew upgrade &&
        ~/.local/share/brew/bin/brew bundle install --global --verbose --no-upgrade -q
      '')
    ];
    home.file.brewfile = {
      target = ".Brewfile";
      text = concatMapStrings (x: "brew \"" + x + "\"\n") config.alex.brew.formulae
        + concatMapStrings (x: "cask \"" + x + "\"\n") config.alex.brew.casks;
      onChange = ''
        brew update &&
        ~/.local/share/brew/bin/brew bundle cleanup --global --zap --force -q &
        brew upgrade &&
        ~/.local/share/brew/bin/brew bundle install --global --verbose --no-upgrade -q
      '';
    };
  };
}
