{ ... }: {
  imports = [ ../../../modules/newsboat ];
  programs.newsboat.browser = "open";
}
