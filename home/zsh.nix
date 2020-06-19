{ pkgs, ... }: {
  home.packages = [ pkgs.fzy ];
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    defaultKeymap = "viins";
    shellAliases = let
      fasd = "${pkgs.fasd}/bin/fasd";
      fasd_cd = "${pkgs.fasd}/bin/fasd_cd";
    in {
      a = "${fasd} -a";
      s = "${fasd} -si"; # show / search / select
      d = "${fasd} -d"; # directory
      f = "${fasd} -f"; # file
      sd = "${fasd} -sid"; # interactive directory selection
      sf = "${fasd} -sif"; # interactive file selection
      z = "${fasd_cd} -d"; # cd, same functionality as j in autojump
      zz = "${fasd_cd} -d -i"; # cd with interactive selection
    };
    initExtra = let
      instantZsh = builtins.fetchurl
        "https://gist.github.com/romkatv/8b318a610dc302bdbe1487bb1847ad99/raw";
    in ''
      source ${instantZsh}
      instant-zsh-pre "%m :: %2~ %B»%b "
      source <(antibody init)
      MODE_CURSOR_VICMD="#fc20bb block"
      MODE_CURSOR_VIINS="#fc20bb blinking bar"
      MODE_CURSOR_SEARCH="#fc20bb steady underline"
      antibody bundle softmoth/zsh-vim-mode
      antibody bundle aperezdc/zsh-fzy
      bindkey '\ea' fzy-cd-widget
      bindkey '^T'  fzy-file-widget
      bindkey '^R'  fzy-history-widget
      bindkey '^P'  fzy-proc-widget
      antibody bundle zdharma/fast-syntax-highlighting
      instant-zsh-post
    '';
    sessionVariables = { "ZSH_TMUX_AUTOSTART" = "true"; };
    dotDir = ".config/zsh";
    oh-my-zsh = {
      enable = true;
      theme = "evan";
      plugins = [ "tmux" "git" "vi-mode" ];
    };
  };
}
