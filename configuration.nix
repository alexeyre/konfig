{ config, lib, pkgs, ... }: {
  imports = [ ../../os/darwin ];
  networking.hostName = "fedbook";
  nix.trustedUsers = [ "alex" ];
  services.nix-daemon.enable = true;
  nixpkgs.system = lib.mkForce "aarch64-darwin";
  nix.extraOptions = ''
    build-users-group = nixbld
    experimental-features = nix-command flakes ca-references
  '';
  # tmux reattach
  home-manager.users."${config.main-user}" = {
  };

  # set the main user of the machine!
  home = home-manager.users.alex;
}
