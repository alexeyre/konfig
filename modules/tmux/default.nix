{ config, lib, pkgs, ... }:
with lib;

let isMac = lib.hasSuffix "darwin" pkgs.system;
in {
  options.alex.tmux.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure tmux";
  };
  config = lib.mkIf config.alex.tmux.enable {
    alex.brew.formulae = lib.mkIf isMac [ "tmux" ];
    programs.tmux = {
      enable = true;
      package = lib.mkIf isMac pkgs.hello;
      keyMode = "vi";
      newSession = true;
      prefix = "C-a";
      terminal = "screen-256color";
      escapeTime = 0;
      disableConfirmationPrompt = true;
      extraConfig =
        "\n      set -g status-bg colour234\n      set -g status-fg colour137\n      set -g status-left ''\n      set -g status-right '#[fg=colour233,bg=colour241,bold] %d/%m #[fg=colour233,bg=colour245,bold] %H:%M:%S '\n      set -g status-right-length 50\n      set -g status-left-length 20\n      setw -g window-status-current-format ' #I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '\n      setw -g window-status-format ' #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '\n    "
        + ''
          # I'm a Vim user, this makes navigation easier
          setw -g mode-keys vi
          unbind-key j
          bind-key t select-pane -D
          unbind-key k
          bind-key n select-pane -U
          unbind-key h
          bind-key h select-pane -L
          unbind-key l
          bind-key s select-pane -R'';
    };
  };
}
