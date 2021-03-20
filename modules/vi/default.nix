{ config, lib, pkgs, ... }:
with lib; {
  options.alex.vi.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure vi.";
  };
  config = mkIf config.alex.vi.enable {
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
      ];
    };
  };
}
