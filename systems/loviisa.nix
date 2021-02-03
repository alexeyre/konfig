{ pkgs, config, ... }: {
  nix.trustedUsers = [ "alex" ];
  imports = [ ../os/darwin <home-manager/nix-darwin> ];
  networking.hostName = "loviisa";
  users.users.alex.name = "Alex Eyre";
  home-manager.users."alex" = (import ../home.nix);
}
