{
  description = "fedbook";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin";
    darwin.url = "github:lnl7/nix-darwin";
    home-manager.url = "github:nix-community/home-manager/release-22.05";

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
