{ config, lib, pkgs, ... }:
with lib; {
  options.alex.games.enable = mkEnableOption "games";
  options.alex.games.battleNet = mkEnableOption "Battle.net";
  config = mkIf config.alex.games.enable {
    alex.brew.casks =
      mkIf (pkgs.hostPlatform.isMacOS && config.alex.games.battleNet)
      [ "battle-net" ];
  };
}
