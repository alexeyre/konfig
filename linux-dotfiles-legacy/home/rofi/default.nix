{ lib, ... }: {
  imports = [ ./override.nix ];
  programs.rofi = {
    enable = true;
    replaceDmenu = true;
    font = "monospace 12";
    theme = builtins.fetchurl {
      url =
        "https://raw.githubusercontent.com/davatorium/rofi-themes/master/User%20Themes/slate.rasi";
      sha256 = "1ahyx31crg82skzr5h4c1xihrr4lya0zysqh3322nxn86mdqv73w";
    };
  };
}
