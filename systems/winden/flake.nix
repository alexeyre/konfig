{
  description = "winden";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    darwin.url = "github:lnl7/nix-darwin/master";
    home-manager.url = "github:nix-community/home-manager/master";


    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, darwin, nixpkgs, home-manager }: {
    darwinConfigurations."winden" = darwin.lib.darwinSystem {
        modules = [
		(home-manager.darwinModules.home-manager)
		./configuration.nix
	];
      };
  };
}
