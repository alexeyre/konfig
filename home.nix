{ config, lib, pkgs, ... }: {
  imports = [ ./modules ];
  xdg.enable = true;
  alex.vi.enable = true;
  alex.chromium.enable = false;
  alex.shell.enable = true;
  alex.git.enable = true;
  home.packages = with pkgs; [ niv gopass nixfmt ];
}
