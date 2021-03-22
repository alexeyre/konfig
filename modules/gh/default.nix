{ config, lib, pkgs, ... }:
with lib;
{
  config = mkIf (config.programs.gh.enable && config.alex.is-mac) {
    # programs.gh.package = pkgs.hello; # would be nice, but programs.gh doesn't expose .package 2021-03-21
    alex.brew.formulae = [ "gh" ];
  };
}
