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
    "veracrypt"
    "macfuse"
    "iina"
    "radio-silence"
    "notion"
    "toggl-track"
  ];
  alex.setapp.enable = true;
  alex.brew.formulae = [ "jupyterlab" ];
  alex.vimari.enable = true;
  alex.brew.taps = [ "homebrew/bundle" "homebrew/core" "homebrew/cask" ];
  programs.zsh.dirHashes = {
    artifacts = "$HOME/.CMVolumes/University/Artifacts";
  };
}
