{ config, lib, pkgs, ... }:
with lib; {
  options.alex.iterm.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure iTerm2";
  };
  config = mkIf (config.alex.iterm.enable && config.alex.is-mac) {
    home.file.itermThemeScript = {
      source = ./theme.py;
      target = "Library/Application Support/iTerm2/Scripts/theme.py";
    };
    alex.brew.casks = [ "iterm2" ];
  };
}
