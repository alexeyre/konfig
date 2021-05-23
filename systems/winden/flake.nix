{
  description = "winden";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    home-manager.url = "github:nix-community/home-manager/release-20.09";

    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, darwin, nixpkgs, home-manager }: {
    darwinConfigurations."winden" = darwin.lib.darwinSystem {
      inputs = { inherit nixpkgs; };
      modules =
        [ home-manager.darwinModules.home-manager {
		home-manager.useGlobalPkgs = true;
	}
	./configuration.nix ];
    };
  };
}
