{ config, lib, pkgs, ... }: {
	home-manager.users."${config.main-user}".programs.brew.casks = [ "alfred" ];
}
