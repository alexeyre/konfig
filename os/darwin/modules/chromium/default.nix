{ pkgs, lib, config, ... }: {
  imports = [ ../../../modules/chromium.nix ];
  programs.chromium.package =
    lib.mkIf config.programs.chromium.enable pkgs.chromium-dummy;
}
