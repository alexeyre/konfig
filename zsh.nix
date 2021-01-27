{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    prezto = {
      enable = true;
      editor.keymap = "vi";
      prompt.theme = "off";
      pmodules = [
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
    dotDir = ".config/zsh";
    envExtra = ''
      export PATH=$HOME/.local/share/brew/sbin:$HOME/.local/share/brew/bin:$PATH
      export ARCHFLAGS='-arch arm64'
      source $HOME/.cargo/env 2>/dev/null
    '';
    shellAliases = {
      "hm" = "home-manager";
    };
  };
  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
    enableZshIntegration = true;
  };
}
