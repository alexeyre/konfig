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
    home.packages = [ pkgs.fasd ];
    programs.zsh = {
      enable = true;
      defaultKeymap = "viins";
      dotDir = ".config/zsh";
      sessionVariables = { EDITOR = "vi"; };
      initExtra = builtins.readFile ./zshrc;
    };
    programs.direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
      enableZshIntegration = true;
    };
  };
}
