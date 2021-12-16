{ config, lib, pkgs, ... }: {
  programs.fish.enable = false;
  programs.zsh.enable = true;

  home-manager.users."${config.main-user}" = {
    home.packages = with pkgs;
      [
        (writeScriptBin "dot" ''
          #!${pkgs.stdenv.shell}
          file=$(${pkgs.findutils}/bin/find $HOME/.local/dot -type f | ${pkgs.fzf}/bin/fzf)
          [ -f $file ] && vi $file
        '')
        exa
      ];
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    programs.dircolors.enable = true;

    programs.zsh = {
      enable = true;
      dotDir = ".config/zsh";
      envExtra = ''
        if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi
        if [ -e $XDG_CONFIG_HOME/zsh/.pk10k.zsh ]; then . $XDG_CONFIG_HOME/zsh/.pk10k.zsh; fi
      '';
      initExtra = ''
        alias ns=nix-shell
        autoload -Uz url-quote-magic
        zle -N self-insert url-quote-magic
      '';
      zplug.enable = true;
      zplug.plugins = [
        {
          name = "agkozak/zsh-z";
        }
        {
          name = "plugins/common-aliases";
          tags = [ "from:oh-my-zsh" ];
        }
        {
          name = "mafredri/zsh-async";
        }
        {
          name = "themes/afowler";
          tags = [ "as:theme" "from:oh-my-zsh" "depth:1" ];
        }
        {
          name = "DarrinTisdale/zsh-aliases-exa";
        }
        {
          name = "jeffreytse/zsh-vi-mode";
        }
      ];
    };

    programs.fish = let
      babelfishTranslate = path: name:
        builtins.readFile (pkgs.runCommand "${name}.fish" {
          nativeBuildInputs = [ pkgs.babelfish ];
        } "${pkgs.babelfish}/bin/babelfish < ${path} > $out;");
    in {
      enable = false;
      loginShellInit = ''
                fenv source /etc/static/bashrc
                [ -e $HOME/.iterm2_shell_integration.fish ] && source $HOME/.iterm2_shell_integration.fish
                fish_vi_key_bindings
              '';
      plugins = [
        {
          name = "fenv";
          src = pkgs.fetchFromGitHub {
            owner = "oh-my-fish";
            repo = "plugin-foreign-env";
            rev = "dddd9213272a0ab848d474d0cbde12ad034e65bc";
            sha256 = "00xqlyl3lffc5l0viin1nyp819wf81fncqyz87jx8ljjdhilmgbs";
          };
        }
        {
          name = "z";
          src = pkgs.fetchFromGitHub {
            owner = "jethrokuan";
            repo = "z";
            rev = "ccb0ac58bc09841eaa2a54bf2aa7e9fb871d0e3f";
            sha256 = "05z6lnkmzbl212cbfp291p63qfzzqp73nkfizsgbmm0fbiqbi74p";
          };
        }
      ];
    };
  };
}
