{ pkgs, config, ... }: {
  imports = [ ../os/darwin ];
  networking.hostName = "winden";
}
