{ config, lib, pkgs, ... }:
with lib; {
  options.alex.brew.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Generate a ~/.Brewfile and sync it on generation change";
  };
  options.alex.brew.formulae = mkOption {
    type = types.listOf types.str;
    default = [ ];
    description = "The brew formula commands to pass to the home Brewfile";
  };
  options.alex.brew.casks = mkOption {
    type = types.listOf types.str;
    default = [ ];
    description = "The brew cask commands to pass to the home Brewfile";
  };
  options.alex.brew.mas = mkOption {
    type = types.listOf types.str;
    default = [ ];
    description = "The brew mas commands to pass to the home Brewfile";
  };
  options.alex.brew.taps = mkOption {
    type = types.listOf types.str;
    default =
      [ "homebrew/bundle" "homebrew/cask" "homebrew/core" "homebrew/services" ];
    description = "The brew tap commands to pass to the home Brewfile";
  };
  options.alex.brew.directory = mkOption {
    type = types.str;
    default = "/opt/homebrew";
    description = "Homebrew install directory";
  };
  config = mkIf (config.alex.brew.enable && config.alex.is-mac) {
    home.packages =
      [ (pkgs.writeScriptBin "sync_brew" config.home.file.brewfile.onChange) ];
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
        arch -arm64e brew bundle install --force --global --verbose --no-upgrade -q &&
        arch -arm64e brew bundle cleanup --global --zap --force -q
      '';
    };
  };
}