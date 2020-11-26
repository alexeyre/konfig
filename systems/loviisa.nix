{ pkgs, config, ...}: { imports = [ ../os/darwin ];
  networking.hostName = "loviisa";
}
