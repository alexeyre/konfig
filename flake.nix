{
  description = "fedbook";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    darwin.url = "github:lnl7/nix-darwin";
    home-manager.url = "github:nix-community/home-manager";

    darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs: {
    darwinConfigurations."fedbook" = darwin.lib.darwinSystem {
      inputs = { inherit nixpkgs; };
      modules = [
        home-manager.darwinModules.home-manager
        { home-manager.useGlobalPkgs = true; }
        ./configuration.nix
      ];
    };
  };
}
