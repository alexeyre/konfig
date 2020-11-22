{ lib, stdenv, fetchurl, ... }:

let
  ver = "0.03";
  name = "lotion";
in stdenv.mkDerivation rec {
  pname = "${name}";
  version = "${ver}";

  src = fetchurl {
    url = "https://github.com/puneetsl/${name}/archive/V-${ver}.tar.gz";
    sha256 = "0k3ycga3b0jnj13whwiip2l0gx32l50pnbh7kfima87nq65aaa5w";
  };

  meta = with lib; {
    description = "Unofficial notion.so client";
    homepage = "https://www.github.com/puneetsl/lotion";
    license = licenses.gpl2;
    platforms = platforms.linux;
  };
}
