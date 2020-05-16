self: super: {
  # rockbox-mod = super.libsForQt5.callPackage (import ./rockbox-mod) { };
  allTheIcons = super.callPackage (import ./allTheIcons) { };
  etBook = super.callPackage (import ./etBook) { };
  croscore = super.callPackage (import ./croscore) { };
  apple-color-emoji = super.callPackage (import ./apple-color-emoji) { };
}
