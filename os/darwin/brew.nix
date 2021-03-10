{ pkgs, ... }: {
  home.packages = [
    (pkgs.writeScriptBin "sync_brew" ''
      brew bundle install --global --verbose --no-upgrade -q
      brew bundle cleanup --global --zap --force -q
    '')
  ];
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
}
