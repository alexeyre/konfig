{ pkgs, ... }: {

  home.packages = [ pkgs.fasd ];
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    prezto = {
      enable = true;
      editor.keymap = "vi";
      pmodules = [
        "fasd"
        "syntax-highlighting"
        "osx"
        "homebrew"
        "environment"
        "completion"
        "terminal"
        "history"
        "history-substring-search"
        "prompt"
        "git"
        "utility"
        "editor"
        "directory"
        "spectrum"
      ];
    };
    # dotDir = ".config/zsh"; # breaks everything for some reason, 2021-02-18
    sessionVariables = {
      HOMEBREW_NO_ANALYTICS = 1;
      HOMEBREW_NO_AUTO_UPDATE = "yes";
      PATH = "$HOME/.local/share/brew/sbin:$HOME/.local/share/brew/bin:$PATH";
      DICPATH = "$HOME/.nix-profile/share/hunspell";
    };
    envExtra = ''
      	    vterm_printf(){
      		    if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
      			    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
      				    elif [ "''${TERM%%-*}" = "screen" ]; then
      				    printf "\eP\e]%s\007\e\\" "$1"
      		    else
      			    printf "\e]%s\e\\" "$1"
      				    fi
      	    }
          if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
      	    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
      		    fi
      		    '';

    shellAliases = {
      "hm" = "home-manager";
      "dr" = "darwin-rebuild";
      "ecn" = "emacsclient -nc";
      "ec" = "emacsclient -n";
      # use emacs more
      "vi" = "emacsclient -n";
      "vim" = "emacsclient -n";
      "nvim" = "emacsclient -n";
    };
  };
  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
    enableZshIntegration = true;
  };
  home.file.hushenv = {
    text = "thisisempty";
    target = ".hushlogin";
  };
}
