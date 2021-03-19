{ pkgs, config, ... }: {
  imports = [ ../os/darwin <home-manager/nix-darwin> ];
  networking.hostName = "winden";
  nix.trustedUsers = [ "alex" ];
  services.nix-daemon.enable = false;

  home-manager.users.alex.alex.brew.taps = [ "fabianishere/personal" ];
  home-manager.users.alex.alex.brew.formulae = [ "pam_reattach" ];
}
