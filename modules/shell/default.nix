{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ./mac.nix ];
  options.alex.shell.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure the z shell";
  };
  config = mkIf config.alex.shell.enable {
    programs.zsh = {
      enable = true;
      defaultKeymap = "viins";
      dotDir = ".config/zsh";
      sessionVariables.EDITOR = "vi";
      initExtra = builtins.readFile ./zshrc;
      dirHashes.dot = "$HOME/.local/dot";
      dirHashes.cls = "$HOME/Projects/classes";
      shellAliases = {
      ls = "ls -G";
      l = "ls -alG";
      cdl = "cd $(dirname $_)";
      };
    };
    programs.direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
      enableZshIntegration = true;
    };
    programs.dircolors.enable = true;
  };
}
