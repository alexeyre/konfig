{ config, lib, pkgs, ... }: {
  programs.chromium.package =
    lib.mkIf config.programs.chromium.enable pkgs.chromium-dummy;
}
