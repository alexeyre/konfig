{ config, pkgs, lib, mkDerivation, qtbase, qttools, qmake, ... }:

{
	nixpkgs.config.allowUnfree = true;
	imports = [
		./overlays.nix
	];
	fonts.fonts = with pkgs; [
		nerdfonts
		allTheIcons
		etBook
		noto-fonts
		noto-fonts-cjk
		noto-fonts-extra
		noto-fonts-emoji
	];
	fonts.fontconfig = {
		penultimate.enable = true;
		defaultFonts = {
			serif = [ "ETBembo" ];
			sansSerif = [ "Noto Sans" ];
			monospace = [ "FiraCode Nerd Font" ];
		};
	};
	console = {
		font = "Lat2-Terminus16";
		keyMap = "dvorak-programmer";
	};
	i18n.defaultLocale = "en_GB.UTF-8";
	time.timeZone = "Europe/London";
	programs.dconf.enable = true;
	programs.adb.enable = true;
	services.udisks2.enable = true;
	services.udev.extraRules = ''
SUBSYSTEM=="usb", ATTR{idVendor}=="05ac", MODE="0666", GROUP="wheel"
	'';
	programs.xss-lock = {
		enable = true;
		lockerCommand = "${pkgs.slock}/bin/slock";
	};
	services.tlp.enable = true;
	programs.slock.enable = true;
	environment.systemPackages = with pkgs; [ 
		home-manager-unstable.home-manager 
		git
		unzip
		xorg.xorgserver
		xorg.xf86videointel
		gnupg
		p7zip
		wireguard-tools
		slock
	];
	environment.pathsToLink = ["${pkgs.xorg.libxcb}/lib/" "/share/zsh"];
	networking.firewall.enable = true;
	networking.firewall.checkReversePath = "loose";
	networking.firewall.allowedUDPPorts = [ 51820 ];
	networking.wireguard.enable = true;
	
        services.xserver = {
                enable = true;
                layout = "us";
                xkbVariant = "dvp";
                xkbOptions = "ctrl:nocaps";
                libinput.enable = true;
		libinput.tapping = false;
                videoDrivers = [ "intel" ];
                desktopManager = {
                        xterm.enable = false;
                };
                windowManager.bspwm.enable = true;
        };
	programs.zsh = {
		enable = true;
	};
        users.users.alex = {
                isNormalUser = true;
                extraGroups = [ "wheel" "audio" "adbusers" "video" ];
		shell = pkgs.zsh;
        };
	system.stateVersion = "19.09";
	services.connman.enable = true;
	networking.wireless = {
		userControlled.enable = true;
		networks = { TANHETOADETHOADETHAOD = {}; };
	};
}
