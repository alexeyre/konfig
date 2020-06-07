{ config, pkgs, lib, ... }:
let cfg = config.programs.st;
in {
  options.programs.st = {
    enable = lib.mkEnableOption "The suckless terminal emulator";
  };
  config = let
    stLuke = (pkgs.st.overrideDerivation (old: rec {
      src = pkgs.fetchFromGitHub {
        owner = "Lukesmithxyz";
        repo = "st";
        rev = "22c71c355ca4f4e965c3d07e9ac37b0da7349255";
        sha256 = "0hnzm0yqbz04y95wg8kl6mm6cik52mrygm8s8p579fikk6vlq3bx";
      };
    }));
  in lib.mkIf cfg.enable {
    home.packages = [ stLuke ];
    home.sessionVariables = { TERMINAL = "${stLuke}/bin/st"; };
  };
}
