{ config, pkgs, ... }:
{
  imports = [ ./zsh.nix ];
  programs.home-manager.enable = true;
  home.username = "alex";
  home.homeDirectory = "/Users/alex";
  home.packages = with pkgs; [ antigen ];
  home.stateVersion = "21.03";
}
