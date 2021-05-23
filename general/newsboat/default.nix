{ config, lib, pkgs, ... }:
with lib; {
  home-manager.users."${config.main-user}" = {
    programs.newsboat = {
      enable = true;
      urls = builtins.map (x: {
        tags = [ "youtube" ];
        url = x;
      }) (lib.splitString "\n" (builtins.readFile ./subs.urls));
      browser = "open";
      extraConfig = ''
        bind-key h down
        bind-key t up
      '';
    };
  };
}
