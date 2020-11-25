{ pkgs, ... }: {
  home.packages = [ pkgs.antigen ];
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    initExtra = let antibody = "${pkgs.antibody}/bin/antibody";
    in ''
      export PATH=$HOME/.local/share/brew/bin:/usr/sbin:$PATH
      if [ -e /Users/alex/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/alex/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
      if type brew &>/dev/null; then
        FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH

        autoload -Uz compinit
        compinit
      fi
      source <(${antibody} init)
      # vim
      KEYTIMEOUT=1
      VIM_MODE_ESC_PREFIXED_WANTED='bdfhul.g'  # Default is 'bdf.g'
      MODE_CURSOR_VIINS="#00ff00 blinking bar"
      MODE_CURSOR_REPLACE="$MODE_CURSOR_VIINS #ff0000"
      MODE_CURSOR_VICMD="green block"
      MODE_CURSOR_SEARCH="#ff00ff steady underline"
      MODE_CURSOR_VISUAL="$MODE_CURSOR_VICMD steady bar"
      MODE_CURSOR_VLINE="$MODE_CURSOR_VISUAL #00ffff"
      antibody bundle softmoth/zsh-vim-mode
      # syntax highlighting
      antibody bundle zdharma/fast-syntax-highlighting
      # additional shell completions
      antibody bundle zsh-users/zsh-completions
      # fish-like completions
      antibody bundle zsh-users/zsh-autosuggestions
      # fasd with macos support
      antibody bundle https://gist.github.com/9aeba3f948eab0e156b704de0878570d.git
      # exa
      antibody bundle DarrinTisdale/zsh-aliases-exa
      # xdd sick rice BRO
      antibody bundle eendroroy/alien-minimal

      # tmux
      ZSH_TMUX_AUTOSTART=true
      # ZSH_TMUX_ITERM2=true
      antibody bundle ohmyzsh/ohmyzsh path:plugins/tmux
    '';
  };
}
