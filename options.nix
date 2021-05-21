{ lib, ... }:
with lib; {
  options.main-user = mkOption {
    type = types.attrsOf types.inferred;
    description = "The home-manager attr set that reflects the main user.";
  };
}
