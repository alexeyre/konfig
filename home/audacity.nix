{ lib, pkgs, ... }: {
  home.packages = [ pkgs.audacity ];
  home.file.mosquito4 = let
    mosquito4 = builtins.fetchurl
      "https://forum.audacityteam.org/download/file.php?id=17751";
  in {
    source = "${mosquito4}";
    target = ".audacity-files/plug-ins/mosquito4.ny";
  };
  home.file.rms-normalise = let
    rms-normalise = builtins.fetchurl
      "https://forum.audacityteam.org/download/file.php?id=20836";
  in {
    source = "${rms-normalise}";
    target = ".audacity-files/plug-ins/rms-normalise.ny";
  };
}
