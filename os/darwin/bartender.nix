{ ... }: {
  home.file.bartenderPreferences = {
    source = ./com.surteesstudios.Bartender-setapp.plist;
    target = "Library/Preferences/com.surteesstudios.Bartender-setapp.plist";
  };
}
