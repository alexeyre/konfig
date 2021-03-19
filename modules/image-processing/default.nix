{ pkgs, ... }: {
  home.packages = [
    (pkgs.writeScriptBin "strip_exif" ''
      #!${pkgs.stdenv.shell}
      ${pkgs.exiftool}/bin/exiftool -all= "$@"
    '')
  ];
}
