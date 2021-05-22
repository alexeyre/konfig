{
  description = "winden";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-20.09-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, darwin, nixpkgs }: {
    darwinConfigurations."winden" = darwin.lib.darwinSystem {
      modules = [ ./configuration.nix ];
      inputs = { inherit nixpkgs self; };
    };
  };
}
