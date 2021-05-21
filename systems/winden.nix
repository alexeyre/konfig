{ pkgs, config, ... }: {
  imports = [ ../os/darwin <home-manager/nix-darwin> ];
  networking.hostName = "winden";
  nix.trustedUsers = [ "alex" ];
  services.nix-daemon.enable = false;

  home-manager.users.alex = (import ../os/darwin/home.nix);

  #nix.extraOptions = ''
  #  extra-platforms = x86_64-darwin aarch64-darwin
  #  system = aarch64-darwin
  #'';
}
