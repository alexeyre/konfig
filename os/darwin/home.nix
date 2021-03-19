{ pkgs, lib, ... }: {
  imports = [
    ../../home.nix
    ./shell.nix
    ./iterm
    ./vimari.nix
    ./bartender.nix
    ./git.nix
    ./brew.nix
  ];
  alex.brew.enable = true;
  alex.brew.casks = [
    "homebrew/cask/programmer-dvorak"
    "alfred"
    "anki"
    "telegram"
    "transmission"
    "setapp"
    "monero-wallet"
    "syncthing"
    "iterm2"
    "veracrypt"
    "macfuse"
  ];
  alex.brew.formulae = [
    "libvterm"
    "fabianishere/personal/pam_reattach"
  ];
}
