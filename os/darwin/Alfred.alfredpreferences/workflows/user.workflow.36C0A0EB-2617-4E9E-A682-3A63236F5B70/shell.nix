{ pkgs ? import <nixpkgs> {} }:
with pkgs.python3Packages;
pkgs.mkShell {
  buildInputs = with pkgs; [
    (buildPythonPackage rec {
      pname = "notion";
      version = "0.0.28";
      src = fetchPypi {
        inherit pname version;
        sha256 = "08fdd5ef7c96480ad11c12d472de21acd32359996f69a5259299b540feba4560";
      };
      progagatedBuildInputs = [ setuptools ];
    })
    python3
  ];
  shellHook = ''
    # Allow the use of wheels.
    SOURCE_DATE_EPOCH=$(date +%s)
  '';
}
