{ ... }: {
  imports = [ ./override.nix ];
  programs.rofi = {
    enable = true;
    replaceDmenu = true;
    font = "monospace 12";
  };
}
