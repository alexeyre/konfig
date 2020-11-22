{ fetchFromGitHub }:
let
  name = "tufte-css";
  version = "v1.7.2";
in fetchFromGitHub {
  owner = "edwardtufte";
  repo = name;
  rev = version;
  sha256 = "18xis2nvviv2yp9w8rjpd9cfc3f92zljimd49jnia4lg7kvqycgj";
  postFetch = ''
    tar xvf $downloadedFile
    mkdir -p $out/share/fonts/truetype
    find . -name "*.ttf" -exec cp {} $out/share/fonts/truetype \;
  '';
}
