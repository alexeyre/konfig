{ pkgs, lib, ... }:
let vimariPrefs = pkgs.writeText "vimariPrefs" ''
{
  "excludedUrls": "",
  "linkHintCharacters": "aoeuihtns",
  "detectByCursorStyle": false,
  "scrollSize": 150,
  "openTabUrl": "https://google.co.uk/",
  "modifier": "",
  "smoothScroll": false,
  "scrollDuration": 25,
  "transparentBindings": true,
  "bindings": {
      "hintToggle": "a",
      "newTabHintToggle": "shift+a",
      "scrollUp": "k",
      "scrollDown": "j",
      "scrollLeft": "h",
      "scrollRight": "l",
      "scrollUpHalfPage": "u",
      "scrollDownHalfPage": "d",
      "goToPageTop": "g g",
      "goToPageBottom": "shift+g",
      "goToFirstInput": "g i",
      "goBack": "shift+h",
      "goForward": "shift+l",
      "reload": "r",
      "tabForward": "shift+k",
      "tabBack": "shift+j",
      "closeTab": "x",
      "openTab": "t"
  }
}
''; in {
  imports = [ ../../home.nix ./zsh.nix ];
  home.username = "alex";
  home.homeDirectory = "/Users/alex";
  home.file.karabiner = {
    source = ./karabiner.json;
    target = ".config/karabiner/karabiner.json";
  };
  home.file.brewfile = {
    source = ./Brewfile;
    target = ".Brewfile";
    onChange = ''
      brew bundle install --global --verbose --no-upgrade
      brew bundle cleanup --global --force 2>/dev/null &>/dev/null
          '';
  };
home.activation.linkMyStuff = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
  cp "${vimariPrefs}" "Library/Containers/net.televator.Vimari.SafariExtension/Data/Library/Application Support/userSettings.json";
'';
  home.packages = [
    (pkgs.writeScriptBin
    "sync_brew"
    ''
      #!${pkgs.stdenv.shell}
      brew bundle install --global --verbose --no-upgrade
      brew bundle cleanup --global --force 2>/dev/null &>/dev/null
        '')
  ];
}
