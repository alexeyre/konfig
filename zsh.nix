{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    prezto = {
      enable = true;
      editor.keymap = "vi";
      prompt.theme = "redhat";
    };
    dotDir = ".config/zsh";
    envExtra = ''
      export PATH=$HOME/.local/share/brew/sbin:$HOME/.local/share/brew/bin:$PATH
      export ARCHFLAGS='-arch arm64'
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
