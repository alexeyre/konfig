{
  imports = [ ../../home.nix ];
  home.username = "alex";
  home.homeDirectory = "/Users/alex";
  home.file.brewfile = {
    source = ./Brewfile;
    target = ".Brewfile";
    onChange = ''
brew bundle install --global --verbose --no-upgrade
brew bundle cleanup --global --force 2>/dev/null &>/dev/null
    '';
  };
}
