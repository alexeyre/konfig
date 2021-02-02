{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    prezto = {
      enable = false;
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
      source $HOME/.cargo/env 2>/dev/null
      source $HOME/.nix-profile/etc/profile.d/nix.sh
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
