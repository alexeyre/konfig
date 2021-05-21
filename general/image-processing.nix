{ config, lib, pkgs, ... }:
with lib; {
  options.alex.image-processing.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Install image scripts";
  };
  config = mkIf config.alex.image-processing.enable {
    home.packages = [
      (pkgs.writeScriptBin "strip_exif" ''
        #!${pkgs.stdenv.shell}
        ${pkgs.exiftool}/bin/exiftool -all= "$@"
      '')
    ];
  };
}
