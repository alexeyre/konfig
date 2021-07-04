{ config, lib, pkgs, ... }: {
  home-manager.users."${config.main-user}" = {
    home.sessionVariables.EDITOR = "vi";
    programs.neovim = {
      enable = true;
      package = pkgs.neovim-nightly;
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
        onehalf
        vim-markdown
        (lib.mkIf
          config.home-manager.users."${config.main-user}".programs.fzf.enable
          fzf-vim)
      ];
    };
  };
}
