self: super: {
  ungoogled-chromium =
    super.lib.mkIf (super.lib.hasSuffix "darwin" builtins.currentSystem)
    (super.callPackage ./ungoogled-chromium-mac.nix.old { });
}
