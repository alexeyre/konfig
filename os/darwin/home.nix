{ pkgs, lib, ... }: {
  imports = [ ../../home.nix ./shell.nix ];
  programs.chromium.package = pkgs.chromium-dummy;
  programs.chromium.extensions = [{
    id = "pdnojahnhpgmdhjdhgphgdcecehkbhfo";
  } # open in iina
    ];
  home.file.karabiner = {
    source = ./karabiner.json;
    target = ".config/karabiner/karabiner.json";
  };
  home.file.bartenderPreferences = {
    source = ./com.surteesstudios.Bartender-setapp.plist;
    target = "Library/Preferences/com.surteesstudios.Bartender-setapp.plist";
  };
  home.file.brewfile = {
    source = ./Brewfile;
    target = ".Brewfile";
    onChange = ''
      #!/bin/zsh
      export HOMEBREW_NO_AUTO_UPDATE="yes"
      ~/.local/share/brew/bin/brew bundle install --global --verbose --no-upgrade -q
      ~/.local/share/brew/bin/brew bundle cleanup --global --zap --force -q
          '';
  };
  programs.git.package = pkgs.hello;
}
