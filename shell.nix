{ config, lib, pkgs, ... }: {
  programs.fish.enable = false;
  programs.zsh.enable = true;

  home-manager.users.alex = {
    home.packages = with pkgs;
      [
        (writeScriptBin "dot" ''
          #!${pkgs.stdenv.shell}
          file=$(${pkgs.findutils}/bin/find $HOME/.local/dot -type f | ${pkgs.fzf}/bin/fzf)
          [ -f $file ] && vi $file
        '')
      ];
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    programs.fzf = { enable = true; };
    programs.dircolors.enable = true;
    programs.starship.enable = true;
    programs.starship.settings = { add_newline = false; };
    programs.zsh = {
      enable = true;
      dotDir = ".config/zsh";
      envExtra = ''
        # if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi
      '';
      shellAliases = {
        ns = "nix-shell";
        nss = "nix search nixpkgs";
      };
      initExtra = ''
        autoload -Uz url-quote-magic
        zle -N self-insert url-quote-magic
      '';
      zplug.enable = true;
      zplug.plugins = [
        # { name = "agkozak/agkozak-zsh-prompt"; }
        { name = "agkozak/zsh-z"; }
        { name = "jeffreytse/zsh-vi-mode"; }
        { name = "unixorn/fzf-zsh-plugin"; }
      ];
      zplug.zplugHome = ~/.local/share/zplug;
    };
  };
}
