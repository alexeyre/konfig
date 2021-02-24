{ pkgs, config, ... }: {
  nix.trustedUsers = [ "alex" ];
  imports = [ ../os/darwin <home-manager/nix-darwin> ];
  networking.hostName = "loviisa";
  home-manager.users."alex" = (import ../home.nix);
}
