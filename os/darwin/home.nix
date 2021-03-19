{ pkgs, lib, ... }: {
  imports = [ ../../home.nix ];
  alex.brew.enable = true;
  alex.tmux.enable = true;
  alex.keyboard.karabiner.enable = true;
  alex.bartender.enable = true;
  alex.iterm.enable = true;
  alex.brew.casks = [
    "homebrew/cask/programmer-dvorak"
    "alfred"
    "anki"
    "telegram"
    "transmission"
    "setapp"
    "monero-wallet"
    "syncthing"
    "veracrypt"
    "macfuse"
  ];
  alex.vimari.enable = true;
  alex.brew.taps = [ "homebrew/bundle" "homebrew/core" "homebrew/cask" ];
}
