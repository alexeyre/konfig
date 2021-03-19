{ ... }: {
  programs.tmux = {
    enable = true;
    keyMode = "vi";
    newSession = true;
    prefix = "C-a";
    terminal = "screen-256color";
    escapeTime = 0;
    disableConfirmationPrompt = true;
    extraConfig =
      "\n      set -g status-bg colour234\n      set -g status-fg colour137\n      set -g status-left ''\n      set -g status-right '#[fg=colour233,bg=colour241,bold] %d/%m #[fg=colour233,bg=colour245,bold] %H:%M:%S '\n      set -g status-right-length 50\n      set -g status-left-length 20\n      setw -g window-status-current-format ' #I#[fg=colour250]:#[fg=colour255]#W#[fg=colour50]#F '\n      setw -g window-status-format ' #I#[fg=colour237]:#[fg=colour250]#W#[fg=colour244]#F '\n    ";
  };
}
