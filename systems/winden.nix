{ pkgs, config, ... }: {
  imports = [ ../os/darwin <home-manager/nix-darwin> ];
  networking.hostName = "winden";
  nix.trustedUsers = [ "alex" ];
  users.users.alex.isHidden = false;
  home-manager.users."alex" = (import ../home.nix);
}
