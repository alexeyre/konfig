{ config, lib, pkgs, ... }: {
  programs.zsh.enable = true;
  home-manager.users.alex = {
    home.packages = with pkgs; [ exa ];
    programs.fzf.enable = true;
    programs.zsh = {
      enable = true;
      dotDir = ".config/zsh";
      shellAliases = {
        ns = "nix-shell";
        nss = "nix search nixpkgs";
      };
      envExtra = ''
        export ZSHZ_DATA=$XDG_DATA_HOME/z/cache
      '';
      zplug.enable = true;
      zplug.plugins = [
        { name = "agkozak/agkozak-zsh-prompt"; }
        { name = "agkozak/zsh-z"; }
        { name = "jeffreytse/zsh-vi-mode"; }
        { name = "unixorn/fzf-zsh-plugin"; }
        {
          name = "jeffreytse/zsh-vi-mode";
        }
        # { name = "MohamedElashri/exa-zsh"; }
      ];
      zplug.zplugHome = ~/.local/share/zplug;
    };
  };
}
