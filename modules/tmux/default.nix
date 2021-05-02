{ config, lib, pkgs, ... }:
with lib; {
  options.alex.tmux.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure tmux";
  };
  config = lib.mkIf config.alex.tmux.enable {
    alex.brew.formulae = lib.mkIf config.alex.is-mac [ "tmux" ];
    programs.tmux.tmuxp.enable = true;
    programs.tmux = {
      enable = true;
      package = lib.mkIf config.alex.is-mac pkgs.hello;
      keyMode = "vi";
      newSession = true;
      prefix = "C-a";
      escapeTime = 0;
      disableConfirmationPrompt = true;
      shell = let
        zsh_arm = (pkgs.writeScriptBin "zsh_arm" ''
          #!${pkgs.stdenv.shell}
          arch -arm64e /bin/zsh
        '');
      in mkIf config.alex.is-mac "${zsh_arm}/bin/zsh_arm";
      plugins = with pkgs.tmuxPlugins; [ nord prefix-highlight ];
      extraConfig = ''
        setw -g window-status-current-format ' #I:#W#F '
        setw -g window-status-format ' #I:#W#F '
        setw -g mode-keys vi
        unbind-key j
        bind-key t select-pane -D
        unbind-key k
        bind-key n select-pane -U
        unbind-key h
        bind-key h select-pane -L
        unbind-key l
        bind-key s select-pane -R
        bind c new-window -c "#{pane_current_path}"
        bind '"' split-window -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
            '';
    };
  };
}
