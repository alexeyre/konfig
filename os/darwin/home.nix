{ pkgs, lib, ... }: {
  imports = [ ../../home.nix ./zsh.nix ];
  home.username = "alex";
  programs.chromium.extensions = [{
    id = "pdnojahnhpgmdhjdhgphgdcecehkbhfo";
  } # open in iina
    ];
  home.homeDirectory = "/Users/alex";
  home.file.karabiner = {
    source = ./karabiner.json;
    target = ".config/karabiner/karabiner.json";
  };
  home.file.brewfile = {
    source = ./Brewfile;
    target = ".Brewfile";
    onChange = ''
      brew bundle install --global --verbose --no-upgrade
      brew bundle cleanup --global --force 2>/dev/null &>/dev/null
          '';
  };
  programs.git.package = pkgs.hello;
  home.packages = with pkgs; [
    # emacsMacport # on hold until emacsMacport supports aarch64
    (pkgs.writeScriptBin "sync_brew" ''
      #!${pkgs.stdenv.shell}
      brew bundle install --global --verbose --no-upgrade
      brew bundle cleanup --global --force 2>/dev/null &>/dev/null
        '')
  ];
}
