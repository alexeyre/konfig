{ pkgs, ... }: {
  home.packages = with pkgs; [ hunspell hunspellDicts.en_GB-large sqlite ];
  programs.emacs = {
    enable = true;
    package = (pkgs.emacsWithPackagesFromUsePackage {
      config = ./config.org;
      package = pkgs.emacsMacport;
      alwaysEnsure = true;
      alwaysTangle = true;
    });
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
