{ pkgs, ... }: {
  home.packages = with pkgs; [
    hunspell
    hunspellDicts.en_GB-large
    sqlite
    brightnessctl
  ];
  programs.emacs = {
    enable = true;
    package = (pkgs.emacsUnstable.overrideDerivation
      (old: { depsBuildTarget = old.depsBuildTarget ++ [ pkgs.findutils ]; }));
  };
  services.emacs = {
    enable = true;
    client.enable = true;
    socketActivation.enable = true;
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
