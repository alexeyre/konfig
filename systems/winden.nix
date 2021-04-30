{ pkgs, config, ... }: {
  imports = [ ../os/darwin <home-manager/nix-darwin> ];
  networking.hostName = "macbook";
  nix.trustedUsers = [ "alex" ];
  services.nix-daemon.enable = false;

  # touch ID
  home-manager.users.alex.alex.brew.taps = [ "fabianishere/personal" ];
  home-manager.users.alex.alex.brew.formulae = [ "pam_reattach" ];

  home-manager.users.alex.alex.brew.casks = [
    # photoshop
    "adobe-creative-cloud"

    # calibre
    "calibre"

    # discord
    "homebrew/cask-versions/discord-canary"
  ];

}
