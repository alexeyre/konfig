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
    programs.brew.formulae = [ "fabianishere/personal/pam_reattach" ];
    home.packages = [
      (pkgs.writeScriptBin "rtorrent-tmux"
        "/usr/bin/arch -arm64 ${pkgs.tmux}/bin/tmux new-session -d -s rtorrent '/usr/bin/arch -arm64 ${pkgs.rtorrent}/bin/rtorrent'")
    ];
  };

  # set the main user of the machine!
  main-user = "alex";
}
