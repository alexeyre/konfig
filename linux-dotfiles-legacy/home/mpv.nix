{
  programs.mpv = {
    enable = true;
    profiles = {
      pip = {
        ontop = true;
        autofit = "720x480";
        geometry = "98%:4%";
      };
    };
    config = {
      profile = "gpu-hq,pip";
      scale = "ewa_lanczossharp";
      cscale = "ewa_lanczossharp";
      video-sync = "display-resample";
      ytdl-format = "bestvideo[height<=?1080]+bestaudio";
      tscale = "oversample";
    };
    bindings = {
      "Ctrl+p" =
        "run bspc -t floating ; run bspc -g sticky=on ; apply-profile pip";
      "]" = "add speed 0.1";
      "[" = "add speed -0.1";
    };
  };
}
