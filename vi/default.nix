{ config, lib, pkgs, ... }: {
  home-manager.users.alex = {
    home.sessionVariables.EDITOR = "vi";
    programs.neovim = {
      enable = true;
      package = pkgs.neovim-unwrapped;
      extraConfig = ''
        source ${pkgs.vimPlugins.vim-plug}/plug.vim
        ${builtins.readFile ./init.vim}
      '';
      viAlias = true;
      vimAlias = true;
      coc.enable = true;
      plugins = with pkgs.vimPlugins; [ vim-plug ];
    };
  };
}
