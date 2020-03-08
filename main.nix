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
	programs.dconf.enable = true;
	programs.adb.enable = true;
	services.udev.extraRules = ''
SUBSYSTEM=="usb", ATTR{idVendor}=="05ac", MODE="0666", GROUP="wheel"
	'';
	environment.systemPackages = with pkgs; [ 
		home-manager-unstable.home-manager 
		git
		unzip
		xorg.xorgserver
		xorg.xf86videointel
		gnupg
		wireguard-tools
		rockbox-mod
		p7zip
	];
	virtualisation.virtualbox.host.enable = true;
	environment.pathsToLink = ["${pkgs.xorg.libxcb}/lib/"];
	services.emacs = {
		enable = true;
		package = pkgs.emacsGit;
		install = true;
		defaultEditor = true;
	};
	networking.firewall.enable = true;
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
                windowManager.bspwm.enable = true;
		desktopManager.plasma5.enable = true;
        };
        users.users.alex = {
                isNormalUser = true;
                extraGroups = [ "wheel" "audio" "adbusers" ];

        };
	system.stateVersion = "19.09";
	services.connman.enable = true;
	networking.wireless.networks = { TANHETOADETHOADETHAOD = {}; };
	networking.wireguard.enable = true;
}
