{ pkgs, ... }: {
  imports = [ ../modules/newsboat ../modules/tmux ];
  home-manager.users."alex".home.packages = with pkgs; [ gopass ];
}
