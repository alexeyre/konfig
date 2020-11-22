{ fetchFromGitHub }:
fetchFromGitHub {
  owner = "google";
  repo = "fonts";
  rev = "113bac0943d5b85dcc6c2c01b48fae88d4b5c52a";
  sha256 = "1qjzpv5gcza2saab6nnxq5k20jwjixdfhsnad4rsf5aj94pcjs9c";
  postFetch = ''
    local out_ttf=$out/share/fonts/truetype
    mkdir -p $out_ttf
    cp apache/tinos/*.ttf $out_ttf
    cp apache/arimo/*.ttf $out_ttf
    cp apache/cousine/*.ttf $out_ttf
  '';
}
