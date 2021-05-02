{ pkgs, ... }: { nixpkgs.overlays = [ (import ./packages) ];
nix.package = pkgs.nixUnstable;
}
