{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    prezto = {
      enable = true;
      editor.keymap = "vi";
    };
    envExtra = ''
      export PATH=$PATH:$HOME/.local/share/brew/bin
    '';
  };
  programs.direnv = {
    enable = true;
    enableNixDirenvIntegration = true;
    enableZshIntegration = true;
  };
}
