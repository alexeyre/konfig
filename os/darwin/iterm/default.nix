{ config, lib, pkgs, ... }:
with lib; {
  main-user.home.file.itermThemeScript = {
    source = ./theme.py;
    target = "Library/Application Support/iTerm2/Scripts/theme.py";
  };
  main-user.brew.casks = [ "iterm2" ];
}
