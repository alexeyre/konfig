{
  description = "winden";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-21.05-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    home-manager.url = "github:nix-community/home-manager/release-21.05";

    # overlays
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";

    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs: {
    darwinConfigurations."winden" = darwin.lib.darwinSystem {
      inputs = { inherit nixpkgs; };
      modules = [
        home-manager.darwinModules.home-manager
        { home-manager.useGlobalPkgs = true; }
        { nixpkgs.overlays = [ inputs.neovim-nightly-overlay.overlay ]; }
        ./configuration.nix
      ];
    };
  };
}
