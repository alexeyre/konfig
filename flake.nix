{
  description = "fedbook";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/c6702166a675c64659dc9b48c7095856c52bccac";
    darwin.url = "github:lnl7/nix-darwin/master";
    home-manager.url = "github:nix-community/home-manager/release-21.11";

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
