{ config, pkgs, lib, ... }:

{
	nixpkgs.config.allowUnfree = true;
	imports = [
		./overlays.nix
		./devices/p50.nix
	];
	fonts.fonts = with pkgs; [
		nerdfonts
	];
	console = {
		font = "Lat2-Terminus16";
		keyMap = "dvorak-programmer";
	};
	i18n.defaultLocale = "en_GB.UTF-8";
	time.timeZone = "Europe/London";
	environment.systemPackages = with pkgs; [ 
		home-manager-unstable.home-manager 
		git
		unzip
		mullvad-vpn
		xorg.xorgserver
		xorg.xf86videointel
	];
	services.emacs = {
		enable = true;
		package = pkgs.emacsGit;
		install = true;
		defaultEditor = true;
	};
        services.xserver = {
                enable = true;
                layout = "us";
                xkbVariant = "dvp";
                xkbOptions = "ctrl:nocaps";
                libinput.enable = true;
                videoDrivers = [ "intel" ];
                desktopManager = {
                        xterm.enable = false;
                };
                displayManager.sddm.enable = true;
                windowManager.bspwm.enable = true;
        };
        users.users.alex = {
                isNormalUser = true;
                extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
        };
	system.stateVersion = "19.09";
	services.connman.enable = true;
	networking.wireless.networks = { TANHETOADETHOADETHAOD = {}; };
	networking.wireguard.enable = true;

}
