{ pkgs, ... }: {
  imports = [ ../../os/darwin ];
  networking.hostName = "winden";
  nix.trustedUsers = [ "alex" ];
  services.nix-daemon.enable = false;

  # set the main user of the machine!
  main-user = "alex";
}
