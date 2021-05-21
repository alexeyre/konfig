{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ./modules ];
  xdg.enable = true;
  alex.vi.enable = true;
  alex.chromium.enable = false;
  alex.shell.enable = true;
  alex.git.enable = true;
  home.packages = with pkgs; [ niv gopass nixfmt ];
  programs.gh.enable = true;
  programs.git.lfs.enable = true;
  alex.newsboat.enable = false;
  alex.fzf.enable = true;
  alex.tex.enable = false;
  alex.fasd.enable = true;
  alex.less.enable = true;

  alex.games.enable = true;
  alex.games.battleNet = true;

  programs.readline = {
    enable = true;
    variables."bell-style" = "none";
  };
}
