{ config, lib, pkgs, ... }:
with lib; {
  home-manager.users."${config.main-user}" = {
    home.file.itermThemeScript = {
      source = ./theme.py;
      target = "Library/Application Support/iTerm2/Scripts/theme.py";
    };
    programs.brew.casks = [ "iterm2" ];
  };
}
