source $(brew --prefix)/share/antigen/antigen.zsh
antigen use oh-my-zsh

if type brew &>/dev/null; then
  FPATH=$(brew --prefix)/share/zsh/site-functions:$FPATH

  autoload -Uz compinit
  compinit
fi

if [[ "$(uname)" =~ "Darwin" ]] ; then
  antigen bundles <<MACOS_BUNDLES
    osx
MACOS_BUNDLES
fi

# library functions
antigen bundle git
antigen bundle command-not-found
antigen bundle docker
antigen bundle plugin-name

# vim
KEYTIMEOUT=1
VIM_MODE_ESC_PREFIXED_WANTED='bdfhul.g'  # Default is 'bdf.g'
MODE_CURSOR_VIINS="#00ff00 blinking bar"
MODE_CURSOR_REPLACE="$MODE_CURSOR_VIINS #ff0000"
MODE_CURSOR_VICMD="green block"
MODE_CURSOR_SEARCH="#ff00ff steady underline"
MODE_CURSOR_VISUAL="$MODE_CURSOR_VICMD steady bar"
MODE_CURSOR_VLINE="$MODE_CURSOR_VISUAL #00ffff"
antigen bundle softmoth/zsh-vim-mode
# syntax highlighting
antigen bundle zdharma/fast-syntax-highlighting
# additional shell completions
antigen bundle zsh-users/zsh-completions
# fish-like completions
antigen bundle zsh-users/zsh-autosuggestions
# fasd with macos support
antigen bundle alex-eyre/9aeba3f948eab0e156b704de0878570d
# exa
antigen bundle DarrinTisdale/zsh-aliases-exa
# xdd sick rice BRO
antigen theme af-magic
# tmux
ZSH_TMUX_AUTOSTART=true
ZSH_TMUX_ITERM2=true
antigen bundle tmux
antigen apply

export PATH="/usr/local/sbin:$PATH"
export PATH="/usr/local/opt/openjdk/bin:$PATH"
if [ -e /Users/alex/.nix-profile/etc/profile.d/nix.sh ]; then . /Users/alex/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
