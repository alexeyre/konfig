{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ./mac.nix ];
  programs.zsh = mkIf (config.alex.shell.shell == "zsh") {
    enable = true;
    defaultKeymap = "viins";
    dotDir = ".config/zsh";
    sessionVariables.EDITOR = "vi";
    initExtra = builtins.readFile ./zshrc;
    dirHashes.dot = "$HOME/.local/dot";
    dirHashes.cls = "$HOME/Projects/classes";
    enableAutosuggestions = true;
    enableCompletion = true;
    oh-my-zsh.enable = true;
    oh-my-zsh.plugins = [ "safe-paste" "fzf" ];
    shellAliases = {
      ls = "ls -G";
      l = "ls -alG";
      cdl = "cd $(dirname $_)";
    };
  };
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
  in mkIf (config.alex.shell.shell == "fish") {
    enable = true;
    interactiveShellInit = ''
      fenv source /etc/static/bashrc
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
}
