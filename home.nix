{ config, pkgs, ... }: {
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
    (import (builtins.fetchTarball
      "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz"))
  ];
  imports = [ ./zsh.nix ./emacs ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  home.packages = with pkgs; [
    #      (haskellPackages.ghcWithPackages (pkgs: [ pkgs.QuickCheck pkgs.brittany ]))
    lldb
    git
    htop
    # latest.rustChannels.nightly.rust
    fasd
    gopass
    nixfmt
    (texlive.combine { inherit (texlive) scheme-minimal dvipng; })
  ];
  programs.tmux = {
    enable = true;
    shortcut = "a";
    keyMode = "vi";
  };
  programs.git = {
    enable = true;
    userName = "Alex Eyre";
    userEmail = "hi@alexey.re";
  };
}
