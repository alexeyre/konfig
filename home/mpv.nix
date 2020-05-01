{

  programs.mpv = {
    enable = true;
    profiles = {
      pip = {
        ontop = true;
        autofit = "384x216";
        geometry = "99%:2%";
      };
    };
    config = {
      profile = "gpu-hq,pip";
      scale= "ewa_lanczossharp";
      cscale = "ewa_lanczossharp";
      video-sync = "display-resample";
      ytdl-format = "bestvideo+bestaudio";
      tscale = "oversample";
    };
    bindings = {
      "Ctrl+p" = "run bspc -t floating ; run bspc -g sticky=on ; apply-profile pip";
    };
  };
}
