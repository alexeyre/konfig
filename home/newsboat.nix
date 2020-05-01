{
  programs.newsboat = {
    enable = true;
    extraConfig = ''
      macro y set browser "mpv --profile=pip %u"; open-in-browser ; set browser "elinks %u"
      unbind-key h
      unbind-key j
      unbind-key k
      unbind-key l

      bind-key h quit
      bind-key j down
      bind-key k up
      bind-key l open
      '';
    queries.youtube-sub-box = "unread == \"yes\" and tags # \"youtube\"";
    urls = [
      # { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCBr_Fu6q9iHYQCh13jmpbrg"; } # Errichto
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCD6VugMZKRhSyzWEWA9W2fg"; } # ssethtzeenta
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCJ24N4O0bP7LGLBDvye7oCA"; } # matt d'avella
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCY7dD6waquGnKTZSumPMTlQ"; } # oxford union
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"; } # lukesmithxyz
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCwgKmJM4ZJQRJ-U5NjvR2dg"; } # commaai archive
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCxkMDXQ5qzYOgXPRnOBrp1w"; } # Mike Zamansky
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCGSGPehp0RWfca-kENgBJ9Q"; } # jreg
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC7SeFWZYFmsm1tqWxfuOTPQ"; } # some scottish man
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCKuDLsO0Wwef53qdHPjbU2Q"; } # William Lin
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw"; } # 3blue1brown
      { tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC52kszkc08-acFOuogFl5jw"; } # tibees

      # twitter
      # { tags = ["twitter"]; url = "https://twitrss.me/twitter_user_to_rss/?user="; }
      { tags = ["twitter"]; url = "https://twitrss.me/twitter_user_to_rss/?user=elonmusk"; } # elonmusk
      { tags = ["twitter"]; url = "https://twitrss.me/twitter_user_to_rss/?user=tferriss"; } # tim ferriss
    ];
  };
}
