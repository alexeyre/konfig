{ config, pkgs, lib, ... }:
{
  home.file.qutebrowser = {
    source = ./config.py;
    target = ".config/qutebrowser/config.py";
  };
  home.file.fourchanX = let
    fourchanX =
      builtins.fetchurl "https://www.4chan-x.net/builds/4chan-X-beta.user.js";
  in {
    source = "${fourchanX}";
    target = ".local/share/qutebrowser/greasemonkey/4chan-X-beta.user.js";
  };
  home.file.oneechan = let
    oneechan = builtins.fetchurl
      "https://github.com/KevinParnell/OneeChan/raw/master/builds/OneeChan.user.js";
  in {
    source = "${oneechan}";
    target = ".local/share/qutebrowser/greasemonkey/OneeChan.js";
  };
  home.file.notion = {
    source = ./notion.so.css;
    target = ".config/qutebrowser/notion.so.css";
  };
}
