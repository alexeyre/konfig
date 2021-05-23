{ config, lib, pkgs, ... }:
with lib; {
  options.programs.brew.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Generate a ~/.Brewfile and sync it on generation change";
  };
  options.programs.brew.formulae = mkOption {
    type = types.listOf types.str;
    default = [ ];
    description = "The brew formula commands to pass to the home Brewfile";
  };
  options.programs.brew.casks = mkOption {
    type = types.listOf types.str;
    default = [ ];
    description = "The brew cask commands to pass to the home Brewfile";
  };
  options.programs.brew.mas = mkOption {
    type = types.listOf types.str;
    default = [ ];
    description = "The brew mas commands to pass to the home Brewfile";
  };
  options.programs.brew.taps = mkOption {
    type = types.listOf types.str;
    default =
      [ "homebrew/bundle" "homebrew/cask" "homebrew/core" "homebrew/services" ];
    description = "The brew tap commands to pass to the home Brewfile";
  };
  options.programs.brew.directory = mkOption {
    type = types.str;
    default = "/opt/homebrew";
    description = "Homebrew install directory";
  };
  config = {
    home.packages =
      [ (pkgs.writeScriptBin "sync_brew" config.home.file.brewfile.onChange) ];
    programs.brew.formulae = mkIf (config.programs.brew.mas != [ ]) [ "mas" ];
    home.file.brewfile = {
      target = ".Brewfile";
      text = (concatMapStrings (x:
        ''tap "'' + x + ''
          "
        '') config.programs.brew.taps) + (concatMapStrings (x:
          ''brew "'' + x + ''
            "
          '') config.programs.brew.formulae) + (concatMapStrings (x:
            ''cask "'' + x + ''
              "
            '') config.programs.brew.casks)
        + (concatMapStrings (x: ''mas "'' + x + "\n") config.programs.brew.mas);

      onChange = ''
        	#!${pkgs.stdenv.shell}
          PATH=$PATH:${config.programs.brew.directory}/bin
          arch -arm64e brew bundle install --force --global --verbose --no-upgrade -q &&
          arch -arm64e brew bundle cleanup --global --zap --force -q
      '';
    };
  };
}
