{ stdenv, fetchurl, alsaLib, gnutar, zlib, libGL, xorg, glib, freetype
, fontconfig, libjack2, autoPatchelfHook }:

stdenv.mkDerivation rec {
  name = "fxkatana-${version}";
  version = "FW4";

  src = ./src.tar.gz;

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [
    gnutar
    alsaLib
    zlib
    libjack2
    xorg.libxcb
    xorg.libX11
    xorg.libXrender
    xorg.libXi
    xorg.libSM
    xorg.libICE
    fontconfig
    libGL
    glib
    freetype
  ];

  unpackPhase = ''
    tar xvf $src
  '';

  installPhase = ''
    install -m755 -D ./KatanaFxFloorBoard_FW4/KATANAFxFloorBoard $out/bin/katanafx
  '';

  meta = with stdenv.lib; {
    homepage = "https://studio-link.com";
    description = "Voip transfer";
    platforms = platforms.linux;
    maintainers = with maintainers; [ makefu ];
  };
}
