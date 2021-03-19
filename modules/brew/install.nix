{ config, lib, pkgs, ... }:
with lib; {
  config = {
    home.file.brew-tarball = lib.mkIf config.alex.brew.enable {
      source = pkgs.fetchFromGitHub {
        owner = "Homebrew";
        repo = "brew";
        rev = "1b61d5a5639d7cf76a1fc3faf02c87f0aef9e64c";
        sha256 = "06xmzm1zq3d2v6byrkwns5kr0n9xf94yf7i441lypnnpszqm75c2";
      };
      target = ".local/share/brew_src";
      onChange = ''
        cp -r ~/.local/share/brew_src/* ~/.local/share/brew/
      '';
    };
  };
}
