format:
	nix run nixpkgs.nixfmt nixpkgs.findutils -c find . -type f -name "*.nix" -exec nixfmt {} \; 2>/dev/null
