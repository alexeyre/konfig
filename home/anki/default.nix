{ pkgs, ... }: {
  home.packages = [ pkgs.anki ];
  home.file.anki-night-mode = let
    night-mode = pkgs.fetchFromGitHub {
      repo = "Anki-Night-Mode";
      owner = "krassowski";
      rev = "2eddb0697f405e451db6993c09281293de7ddf11";
      sha256 = "08xrcvq8aipxzk062xnazz372zb368grlcx83a1wq1yr09lljldf";
    };
  in {
    source = "${night-mode}/night_mode";
    target = ".local/share/Anki2/addons21/1496166067";
    recursive = true;
  };
}
