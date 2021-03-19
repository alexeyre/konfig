{ config, lib, pkgs, ... }:
with lib; {
  options.alex.tex.enable = mkOption {
    type = types.bool;
    default = false;
    description =
      "Whether to configure a tex environment with custom additions";
  };
  config = lib.mkIf config.alex.tex.enable {
    home.packages = with pkgs;
      [
        (texlive.combine {
          inherit (texlive)
            minted fvextra scheme-full wrapfig ulem amsmath capt-of hyperref;
        })
      ];
  };
}
