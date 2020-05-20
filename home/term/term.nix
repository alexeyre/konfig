{ config, lib, pkgs, fetchurl, ... }:

{
  imports = [ ./st.nix ./konsole.nix ];
  programs.kitty = {
    enable = false;
    font.package = pkgs.nerdfonts;
    font.name = "FiraCode Nerd Font";
    settings = { font_size = "11.0"; };
  };
  programs.st.enable = true;
  programs.konsole.enable = false;
}
