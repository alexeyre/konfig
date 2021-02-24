self: super: {
  ungoogled-chromium-mac =
    (super.callPackage ./ungoogled-chromium-mac.nix.old { });
  chromium-dummy = (super.callPackage ./chromium-dummy.nix { });
}
