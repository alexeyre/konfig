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
    default = [ ];
    description = "The brew formula commands to pass to the home Brewfile";
  };
  options.alex.brew.casks = mkOption {
    type = types.listOf types.string;
    default = [ ];
    description = "The brew cask commands to pass to the home Brewfile";
  };
  options.alex.brew.mas = mkOption {
    type = types.listOf types.string;
    default = [ ];
    description = "The brew mas commands to pass to the home Brewfile";
  };
  options.alex.brew.taps = mkOption {
    type = types.listOf types.string;
    default =
      [ "homebrew/bundle" "homebrew/cask" "homebrew/core" "homebrew/services" ];
    description = "The brew tap commands to pass to the home Brewfile";
  };
  config = mkIf (config.alex.brew.enable && config.alex.is-mac) {
    home.packages = [
      (pkgs.writeScriptBin "sync_brew" ''
        PATH=$PATH:/opt/homebrew/bin
        brew bundle install --global --verbose --no-upgrade -q &&
        brew bundle cleanup --global --zap --force -q
      '')
    ];
    alex.brew.formulae = mkIf (config.alex.brew.mas != [ ]) [ "mas" ];
    home.file.brewfile = {
      target = ".Brewfile";
      text = (concatMapStrings (x:
        ''tap "'' + x + ''
          "
        '') config.alex.brew.taps) + (concatMapStrings (x:
          ''brew "'' + x + ''
            "
          '') config.alex.brew.formulae) + (concatMapStrings (x:
            ''cask "'' + x + ''
              "
            '') config.alex.brew.casks)
        + (concatMapStrings (x: ''mas "'' + x + "\n") config.alex.brew.mas);

      onChange = ''
        PATH=$PATH:/opt/homebrew/bin
        brew bundle install --global --verbose --no-upgrade -q &&
        brew bundle cleanup --global --zap --force -q
      '';
    };
  };
}
