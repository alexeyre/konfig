{ config, pkgs, lib, ... }: {
  programs.emacs = {
    enable = true;
    package = pkgs.emacsUnstable;
  };
  home.file.init = {
    source = ./init.el;
    target = ".config/emacs/init.el";
  };
  home.file.config = {
    source = ./config.org;
    target = ".config/emacs/config.org";
  };
}
