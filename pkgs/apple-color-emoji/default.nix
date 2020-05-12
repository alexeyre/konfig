{ fetchFromGitHub }:
let
  name = "apple-color-emoji";
  version = "180e627f1614d75e7b54f2b5e107532ad737bf29";
in fetchFromGitHub {
  owner = "potyt";
  repo = "fonts";
  rev = version;
  sha256 = "120ppihhkdyz34fvyh2ha0q42cipkcy9yhabzdi42qcxjya2bzk6";
  postFetch = ''
    tar xvf $downloadedFile
    mkdir -p $out/share/fonts/truetype
    cp "fonts-${version}/macfonts/Apple Color Emoji/Apple Color Emoji.ttf" $out/share/fonts/truetype
  '';
}
