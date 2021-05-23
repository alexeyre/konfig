{ lib, ... }:
with lib; {
  options.main-user = mkOption {
    type = types.str;
    description = "The home-manager user string that reflects the main user.";
  };
}
