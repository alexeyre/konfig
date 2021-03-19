# 10ms for key sequences
KEYTIMEOUT=1
# Basic auto/tab complete:
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files
autoload -U colors && colors	# Load colors
setopt autocd		# Automatically cd into typed directory.
stty stop undef		# Disable ctrl-s to freeze terminal.
setopt interactive_comments
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
        vicmd) echo -ne '\e[1 q' ;;      # block
        viins|main) echo -ne '\e[5 q' ;; # beam
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
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
