{ config, pkgs, ... }:

{
  imports = [ ./zsh.nix ./emacs ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.username = "alex";
  home.homeDirectory = "/Users/alex";
  home.packages = with pkgs;
    [ (haskellPackages.ghcWithPackages (pkgs: [ pkgs.QuickCheck ])) ];
  programs.tmux = {
    enable = true;
    shortcut = "a";
  };
}
