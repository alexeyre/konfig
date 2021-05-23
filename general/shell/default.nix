{ config, lib, pkgs, ... }: {
  programs.fish.enable = true;
  home-manager.users."${config.main-user}" = {
    programs.direnv = {
      enable = true;
      enableNixDirenvIntegration = true;
    };
    programs.dircolors.enable = true;

    programs.fish = let
      babelfishTranslate = path: name:
        builtins.readFile (pkgs.runCommand "${name}.fish" {
          nativeBuildInputs = [ pkgs.babelfish ];
        } "${pkgs.babelfish}/bin/babelfish < ${path} > $out;");
    in {
      enable = true;
      loginShellInit = ''
	fenv source /etc/static/bashrc
        [ -e $HOME/.iterm2_shell_integration.fish ] && source $HOME/.iterm2_shell_integration.fish
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
