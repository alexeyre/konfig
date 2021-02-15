{ stdenv, fetchurl, unzip }:
stdenv.mkDerivation rec {
  name = "ungoogled-chromium";
  pname = "ungoogled-chromium";
  version = "88.0.4324.96";

  buildInputs = [ unzip ];
  sourceRoot = ".";
  phases = [ "unpackPhase" "installPhase" ];
  installPhase = ''
    mkdir -p "$out/Applications"
    cp -r Chromium.app "$out/Applications/Chromium.app"
  '';

  src = fetchurl {
    name = "Chromium-${version}.app.zip";
    url =
      "https://github.com/macchrome/macstable/releases/download/v${version}-r827102-macOS/Chromium.${version}.sync.arm64.app.zip";
    sha256 = "0islnrw6q8iz47nd2wfxv7z807kg0fvq7zagvp22yqh5jalz0n16";
  };

  meta = with stdenv.lib; {
    description = "Ungoogled Chromium";
    homepage = "";
    maintainers = [ ];
    platforms = platforms.darwin;
  };
}
