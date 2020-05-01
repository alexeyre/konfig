{ stdenv, pkgs }:
let
  version = "8.0.7";
in
(import ./nativefier.nix {
  inherit pkgs;
  inherit (stdenv.hostPlatform) system;
}).nativefier.override rec {
  meta = {
    description = "Nativeizes web pages";
    homepage = "https://github.com/jiahaog/nativefier";
  };
}
