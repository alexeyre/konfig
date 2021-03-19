{ config, lib, pkgs, ... }: {
  programs.git.package = lib.mkIf config.programs.git.enable pkgs.hello;
}
