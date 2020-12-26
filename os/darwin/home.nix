{ pkgs, ... }: {
  imports = [ ../../home.nix ];
  home.username = "alex";
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
  home.packages = [
    (pkgs.writeScriptBin
    "sync_brew"
    ''
      #!${pkgs.stdenv.shell}
      brew bundle install --global --verbose --no-upgrade
      brew bundle cleanup --global --force 2>/dev/null &>/dev/null
        '')
  ];
}
