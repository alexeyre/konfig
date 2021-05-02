{ config, lib, pkgs, ... }:
with lib; {
  options.alex.dots = mkOption {
    type = types.path;
    default = builtins.toPath "${config.home.homeDirectory}/.local/dot";
    description = "dotfiles directory";
    readOnly = true;
  };
}
