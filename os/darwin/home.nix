{ pkgs, lib, ... }: {
  imports = [
    ../../home.nix
    ./shell.nix
    ./iterm
    ./vimari.nix
    ./bartender.nix
    ./git.nix
    ./brew.nix
    ./chromium.nix
  ];
}
