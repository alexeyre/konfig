{ config, lib, pkgs, ... }:
with lib; {
  home.file.hushenv = {
    text = "thisisempty";
    target = ".hushlogin";
  };
  programs.zsh.sessionVariables = {
    HOMEBREW_NO_ANALYTICS = 1;
    PATH = "/opt/homebrew/sbin:/opt/homebrew/bin:$PATH";
  };

  programs.zsh.envExtra = ''
    test -e /Users/alex/.config/zsh/.iterm2_shell_integration.zsh && source /Users/alex/.config/zsh/.iterm2_shell_integration.zsh || true
    fpath=(/opt/homebrew/share/zsh/site-functions $fpath)
  '';

  programs.zsh.shellAliases = {
    "hm" = "home-manager";
    "dr" = "darwin-rebuild";
    "drs" = "darwin-rebuild switch 2>/dev/null";
    "drsd" = "darwin-rebuild switch";
  };
}
