{ fetchFromGitHub }:

let
  name = "all-the-icons.el";
  version = "3.2.0";
in fetchFromGitHub {
  name = "${name}";
  owner = "domtronn";
  repo = "all-the-icons.el";
  rev = "${version}";
  sha256 = "10sx6m32210py3kb4xgf2d6a4w2lnkcm62w2ghmq12vc0i1vir66";

  postFetch = ''
    tar xvf $downloadedFile
    mkdir -p $out/share/fonts/truetype
    cp ${name}-${version}/fonts/*.ttf $out/share/fonts/truetype
  '';

  meta = { description = "fonts for allTheIcons"; };
}
