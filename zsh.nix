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
      SPACESHIP_CHAR_SYMBOL="$ "
    '';
    initExtra = ''
      source <(${pkgs.antibody}/bin/antibody init)
      antibody bundle denysdovhan/spaceship-prompt
      eval spaceship_vi_mode_enable
    '';
    shellAliases = {
      "hm" = "home-manager";
      "dr" = "darwin-rebuild";
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
