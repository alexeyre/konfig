{ pkgs, config, ... }: {
  imports = [ ../os/darwin <home-manager/nix-darwin> ];
  networking.hostName = "winden";
  nix.trustedUsers = [ "alex" ];
  services.nix-daemon.enable = false;

  # touch ID
  home-manager.users.alex.alex.brew.taps = [ "fabianishere/personal" ];
  home-manager.users.alex.alex.brew.formulae = [ "pam_reattach" ];

  # photoshop
  home-manager.users.alex.alex.brew.casks = [ "adobe-creative-cloud" ];

  # for inf1b
  home-manager.users.alex.alex.java.enable = true;
  home-manager.users.alex.alex.java.bloat = true;

}
