{ lib, ... }:
with lib; {
  options.home = mkOption {
    type = types.attrs;
    description = "The home-manager set";
  };
}
