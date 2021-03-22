{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ./mac.nix ];
  options.alex.shell.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure the z shell";
  };
  config = mkIf config.alex.shell.enable {
    programs.fzf.enable = true;
    programs.zsh = {
      enable = true;
      defaultKeymap = "viins";
      dotDir = ".config/zsh";
      sessionVariables.EDITOR = "vi";
      initExtra = builtins.readFile ./zshrc;
      dirHashes.dot = "$HOME/.local/dot";
      shellAliases.ls = "ls -G";
      shellAliases.l = "ls -alG";
    };
    programs.direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
      enableZshIntegration = true;
    };
  };
}
