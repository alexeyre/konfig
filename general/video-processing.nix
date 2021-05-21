{ config, lib, pkgs, ... }:
with lib; {
  options.alex.video-processing.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to install video processing scripts";
  };
  config = mkIf config.alex.video-processing.enable {
    home.packages = [
      (pkgs.writeScriptBin "compress_video" ''
        #!${pkgs.stdenv.shell}
        file_name="''${1##*/}"
        file_name="''${file_name%.*}"
        file_name="''${file_name}.mp4"
        dir_name="$(dirname $1)"
        full_path="''${dir_name}/''${file_name}"
        test "$(file -i $1 | grep video)" = "video" && echo ${pkgs.ffmpeg} -i $1 -vcodec libx265 -crf 28 -tag:v hvc1 "''${full_path}"
      '')
    ];
  };
}
