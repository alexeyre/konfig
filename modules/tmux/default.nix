{ config, lib, pkgs, ... }:
with lib;
{
  options.alex.tmux.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure tmux";
  };
  config = lib.mkIf config.alex.tmux.enable {
    alex.brew.formulae = lib.mkIf config.alex.is-mac [ "tmux" ];
    programs.tmux = {
      enable = true;
      package = lib.mkIf config.alex.is-mac pkgs.hello;
      keyMode = "vi";
      newSession = true;
      prefix = "C-a";
      escapeTime = 0;
      disableConfirmationPrompt = true;
      shell = let zsh_arm = (pkgs.writeScriptBin "zsh_arm" ''
        #!${pkgs.stdenv.shell}
        arch -arm64e /bin/zsh
      ''); in mkIf config.alex.is-mac "${zsh_arm}/bin/zsh_arm";
      extraConfig = ''
        set -g status-bg colour234
        set -g status-fg colour137
        set -g status-left ""
        set -g status-right '#[fg=colour233,bg=colour241,bold] %d/%m #[fg=colour233,bg=colour245,bold] %H:%M:%S '
        set -g status-right-length 50
        set -g status-left-length 20
        setw -g window-status-current-format ' #I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '
        setw -g window-status-format ' #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '
        setw -g mode-keys vi
        unbind-key j
        bind-key t select-pane -D
        unbind-key k
        bind-key n select-pane -U
        unbind-key h
        bind-key h select-pane -L
        unbind-key l
        bind-key s select-pane -R
            '';
    };
  };
}
