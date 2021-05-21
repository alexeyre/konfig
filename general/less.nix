{ pkgs, config, lib, ... }:
with lib; {
  options.alex.less.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Wether to configure the less pager";
  };
  config = mkIf config.alex.less.enable {
    home.packages = with pkgs; [ less ];
    home.file.lessKeyConfig = mkIf (config.alex.keyboard.layout == "dvp") {
      text = ''
        #command
        d left-scroll
        h forw-line
        t back-line
        n right-scroll
      '';
      target = ".lesskey";
      onChange = "${pkgs.less}/bin/lesskey";
    };
  };
}
