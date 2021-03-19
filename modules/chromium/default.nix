{ config, lib, pkgs, ... }:
with lib;
let isMac = lib.hasSuffix "darwin" pkgs.system;
in {
  options.alex.chromium.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure and install chromium";
  };
  config = lib.mkIf config.alex.chromium.enable {
    programs.chromium = {
      enable = true;
      package = lib.mkIf isMac pkgs.chromium-dummy;
      extensions = [
        { id = "cgbcahbpdhpcegmbfconppldiemgcoii"; } # ublock dev
        { id = "eckgcipdkhcfghnmincccnhpdmnbefki"; } # umatrix dev
        { id = "kkhfnlkhiapbiehimabddjbimfaijdhk"; } # gopass bridge
        { id = "ohnjgmpcibpbafdlkimncjhflgedgpam"; } # 4chanX
      ];
    };
    alex.brew.casks = lib.mkIf isMac [ "chromium" ];
  };
}
