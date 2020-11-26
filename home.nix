{ config, pkgs, ... }:
{
  imports = [ ./zsh.nix ./emacs ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.packages = with pkgs;
    [
      (haskellPackages.ghcWithPackages (pkgs: [ pkgs.QuickCheck ]))
      lldb
      exa
      git
      htop
    ];
  programs.tmux = {
    enable = true;
    shortcut = "a";
  };
}
