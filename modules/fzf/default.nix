{ config, lib, pkgs, ... }:
with lib;
{
  options.alex.fzf.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure the fzf fuzzy finder";
  };
  options.alex.fzf.configure-shell = mkOption {
    type = types.bool;
    default = true;
    description = "Whether to configure zsh for fzf";
  };
  options.alex.fzf.directory = mkOption {
    type = types.path;
    default = if config.alex.is-mac then "/opt/homebrew/opt/fzf" else "${pkgs.fzf}";
    readOnly = true;
    description = "FZF installation directory";
  };
  config = lib.mkIf config.alex.fzf.enable {
    alex.brew.formulae = mkIf config.alex.is-mac [ "fzf" ];
    home.packages = mkIf (!config.alex.is-mac) [ pkgs.fzf ];
    programs.zsh.initExtra = mkIf config.alex.fzf.configure-shell ''
      source ${config.alex.fzf.directory}/shell/key-bindings.zsh
      bindkey -r "^T"
      bindkey '^O' fzf-file-widget
    '';
    programs.neovim.extraConfig = lib.mkIf config.alex.is-mac ''
      set rtp+=/opt/homebrew/opt/fzf
    '';
  };
}
