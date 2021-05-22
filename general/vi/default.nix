{ config, lib, pkgs, ... }:
with lib; {
  programs.neovim = {
    enable = true;
    extraConfig = builtins.readFile ./init.vim;
    viAlias = true;
    vimAlias = true;
    plugins = with pkgs.vimPlugins; [
      vim-polyglot
      vim-surround
      nerdtree
      goyo
      vimagit
      vimwiki
      vim-airline
      vim-commentary
      vim-css-color
      vim-markdown
      (mkIf config.alex.fzf.enable fzf-vim)
    ];
  };
}
