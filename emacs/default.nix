{ pkgs, ... }: {
  home.packages = with pkgs; [ hunspell hunspellDicts.en_GB-large sqlite imagemagick_light ];
  programs.emacs = {
    enable = true;
    package = pkgs.emacsMacport;
  };
  home.file.init = {
    source = ./init.el;
    target = ".config/emacs/init.el";
  };
  home.file.config = {
    source = ./config.org;
    target = ".config/emacs/config.org";
  };
  home.file.early-init = {
    source = ./early-init.el;
    target = ".config/emacs/early-init.el";
  };
}
