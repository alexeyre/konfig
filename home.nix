{ config, lib, pkgs, ... }:
with lib;
{
  imports = [ ./modules ];
  xdg.enable = true;
  alex.vi.enable = true;
  alex.chromium.enable = false;
  alex.shell.enable = true;
  alex.git.enable = true;
  home.packages = with pkgs; [ niv gopass nixfmt ];
  programs.gh.enable = true;
  programs.git.lfs.enable = true;
  alex.newsboat.enable = true;
  alex.fzf.enable = true;
  alex.tex.enable = true;
  alex.fasd.enable = true;
}
