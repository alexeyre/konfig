{ config, lib, pkgs, fetchurl, ... }:

{
  imports = [ ./st.nix ./konsole.nix ];
  programs.kitty = {
    enable = true;
    font.name = "FiraCode Nerd Font";
    settings = { font_size = "12.0"; };
  };
  home.sessionVariables.TERMINAL = "${pkgs.kitty}/bin/kitty";
  programs.st.enable = false;
  programs.konsole.enable = false;
}
