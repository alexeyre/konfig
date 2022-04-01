{ config, lib, pkgs, ... }: {
  home-manager.users.alex = {
    fonts.fontconfig = {
      enable = true;

    };
    home.file.fontconfig = {
      target = ".config/fontconfig/fonts.conf";
      text = ''
        <?xml version="1.0" encoding="UTF-8"?>
        <!DOCTYPE fontconfig SYSTEM "fonts.dtd">
        <fontconfig>
        <match target="pattern">
        <test qual="any" name="family">
         <string>Terminus (TTF)</string>
        </test>
        <edit name="antialias" mode="assign">
         <bool>false</bool>
        </edit>
        </match>
        </fontconfig>
      '';
    };
  };
}
