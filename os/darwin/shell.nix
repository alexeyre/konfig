{ ... }: {
  programs.zsh.envExtra = ''
    test -e /Users/alex/.config/zsh/.iterm2_shell_integration.zsh && source /Users/alex/.config/zsh/.iterm2_shell_integration.zsh || true
    fpath=($HOME/.local/share/brew/share/zsh/site-functions $fpath)
  '';

  programs.zsh.shellAliases = {
    "hm" = "home-manager";
    "dr" = "darwin-rebuild";
    "drs" = "darwin-rebuild switch 2>/dev/null";
    "drsd" = "darwin-rebuild switch";
  };
}
