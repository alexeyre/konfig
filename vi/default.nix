{ config, lib, pkgs, ... }: {
  home-manager.users.alex = {
    home.sessionVariables.EDITOR = "vi";
    programs.neovim = {
      enable = true;
      # package = pkgs.neovim-unwrapped;
      extraConfig = builtins.readFile ./init.vim;
      viAlias = true;
      vimAlias = true;
      withPython3 = false;
      withRuby = false;
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
        onehalf
        vim-markdown
        (lib.mkIf config.home-manager.users.alex.programs.fzf.enable fzf-vim)
      ];
    };
  };
}
