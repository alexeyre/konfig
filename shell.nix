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
    initExtra = builtins.readFile ./zshrc;
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
