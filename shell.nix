{ pkgs, ... }: {

  programs.fzf.enable = true;
  home.packages = [ pkgs.fasd ];
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    dotDir = ".config/zsh"; # breaks everything for some reason, 2021-02-18
    sessionVariables = {
      HOMEBREW_NO_ANALYTICS = 1;
      PATH = "$HOME/.local/share/brew/sbin:$HOME/.local/share/brew/bin:$PATH";
      EDITOR = "vi";
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
    initExtra = ''
      # Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files
      PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
# Use vim keys in tab complete menu:
bindkey -M menuselect 'd' vi-backward-char
bindkey -M menuselect 't' vi-up-line-or-history
bindkey -M menuselect 'n' vi-forward-char
bindkey -M menuselect 'h' vi-down-line-or-history
bindkey -v '^?' backward-delete-char
      bindkey '^h' up-line-or-history
      bindkey '^t' down-line-or-history
      autoload edit-command-line; zle -N edit-command-line
      bindkey "^e" edit-command-line
      # Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.
    '';
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
