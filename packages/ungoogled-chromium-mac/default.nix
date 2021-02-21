{ stdenv, pkgs, fetchFromGitHub, undmg, ninja, coreutils, readline, xz, zlib
, python27, nodejs }:
stdenv.mkDerivation rec {
  name = "ungoogled-chromium";
  pname = "ungoogled-chromium";
  version = "88.0.4324.150-1.1";

  buildInputs = [ ninja coreutils readline xz zlib python27 nodejs undmg ];
  sourceRoot = "./ungoogled-chromium-macos";
  installPhase = ''
    mkdir -p "$out/Applications"
    cp -r out/Default/Chromium.app "$out/Applications/Chromium.app"
  '';
  unpackPhase = ''
    rm -rf $sourceRoot/ungoogled-chromium
    cp -r ./ungoogled-chromium $sourceRoot/ungoogled-chromium
  '';
  srcs = [
    (fetchFromGitHub {
      owner = "ungoogled-software";
      repo = "ungoogled-chromium-macos";
      rev = version;
      sha256 = "083pzxwa1zy1p4j8vpdbkyiyqlmpwhl0m8psqxy2fgj5nw2qnn3v";
    })
    (fetchFromGitHub {
      owner = "Eloston";
      repo = "ungoogled-chromium";
      rev = "f7c33ded1674556b6e9e72d597c706666f490da8";
      sha256 = "0hzap19pbnfcskpzbqq7dqrankmlrq9q7m1xrf7aygqiir0ksp4y";
    })
  ];

  meta = with pkgs.lib; {
    description = "Ungoogled Chromium";
    homepage = "";
    maintainers = [ ];
    platforms = platforms.darwin;
  };
}
