{ config, pkgs, ... }: {
  imports = [ ../../os/darwin <home-manager/nix-darwin> ];
  networking.hostName = "winden";
  nix.trustedUsers = [ "alex" ];
  services.nix-daemon.enable = false;

  # set the main user of the machine!
  main-user = config.home-manager.users.alex;
}
