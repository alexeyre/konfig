{ pkgs, ... }: {
  programs.zsh = {
    enable = true;
    defaultKeymap = "viins";
    prezto = {
      enable = true;
      editor.keymap = "vi";
    };
    envExtra = ''
      export PATH=$HOME/.local/share/brew/bin:$PATH
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
