{
  description = "winden";
  inputs = {
    darwin.url = "github:lnl7/nix-darwin/master";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, darwin, nixpkgs, home-manager }: {
    darwinConfigurations."winden" = darwin.lib.darwinSystem {
      modules = [
        ./configuration.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.backupFileExtension = ".backup";
        }
      ];
      inputs = { inherit self nixpkgs home-manager; };
    };
  };
}
