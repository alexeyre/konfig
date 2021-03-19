{ pkgs, ... }: {
  home.packages = [ (pkgs.writeScriptBin "sync_brew" "") ];
  home.file.brewfile = {
    source = ./Brewfile;
    target = ".Brewfile";
    onChange = ''
      #!/bin/zsh
      # export HOMEBREW_NO_AUTO_UPDATE="yes"

      brew bundle install --global --verbose --no-upgrade -q
      brew bundle cleanup --global --zap --force -q
          '';
  };
}
