{ pkgs, stdenv, fetchurl, undmg }:
stdenv.mkDerivation rec {
  name = "ungoogled-chromium";
  pname = "ungoogled-chromium";
  version = "88.0.4324.150-1.1";
  phases = [ ];
  src = fetchurl {
    url = "mirror://gnu/hello/${pname}-${version}.tar.gz";
    sha256 = "0ssi1wpaf7plaswqqjwigppsg5fyh99vdlb9kzl7c9lng89ndq1i";
  };
  meta = with pkgs.lib; {
    description = "Ungoogled Chromium";
    homepage = "";
    maintainers = [ ];
    platforms = platforms.darwin;
  };
}
