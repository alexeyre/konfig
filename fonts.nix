{ config, lib, pkgs, ... }: {
  home-manager.users.alex = {
    fonts.fontconfig = {
      enable = true;

    };
    home.files.fontconfig = {
      target = ".config/fontconfig/fonts.conf";
      text = ''
                <fontconfig> 

          <match target="font">
            <test name="family" qual="any">
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
