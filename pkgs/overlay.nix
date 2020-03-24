self: super: {
	rockbox-mod = super.libsForQt5.callPackage (import ./rockbox-mod) { };
}
