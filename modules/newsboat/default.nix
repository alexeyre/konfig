{ lib, config, ... }: {
  programs.newsboat = {
    enable = true;
    urls = builtins.map (x: {
      tags = [ "youtube" ];
      url = x;
    }) (lib.splitString "\n" (builtins.readFile ./subs.urls));
    browser = "open";
    extraConfig = let def = config.programs.newsboat.browser;
    in ''
      bind-key h down
      bind-key t up
      macro v set browser iina ; open-in-browser ; set browser ${def}
    '';
  };
}
