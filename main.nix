{ ... }: {
  nixpkgs.overlays = [ (import ./packages) ];
  users.users.alex = {
    name = "alex";
    home = "/Users/alex";
  };
}
