# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
	hardware.bluetooth.enable = true;
	hardware.pulseaudio.enable = true;
	nixpkgs.config.pulseaudio = true;
	environment.systemPackages = with pkgs; [ pavucontrol ];
	networking.hostName = "memepad";
	hardware.bumblebee = {
		enable = true;
		connectDisplay = true;
		pmMethod = "bbswitch";
		driver = "nouveau";
	};


	boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
	boot.initrd.kernelModules = [ "dm-snapshot" ];
	boot.kernelModules = [ "kvm-intel" ];
	boot.extraModulePackages = [ ];

	fileSystems."/" =
	{ device = "/dev/disk/by-uuid/1cc49fed-355d-4190-9cb3-cc63564050b6";
		fsType = "btrfs";
	};

	fileSystems."/boot/efi" =
	{ device = "/dev/disk/by-uuid/3040-B4CD";
		fsType = "vfat";
	};
	hardware.enableRedistributableFirmware = true;

	swapDevices =
		[ { device = "/dev/disk/by-uuid/e71b529b-84fb-416e-b832-093ae522750f"; }
		];

		nix.maxJobs = lib.mkDefault 8;
		powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";


		boot.loader.systemd-boot.enable = true;
		boot.loader.efi.canTouchEfiVariables = true;
		boot.loader.efi.efiSysMountPoint = "/boot/efi";
		boot.loader.grub = {
			enable = true;
			device = "nodev";
			version = 2;
			efiSupport = true;
			enableCryptodisk = true;
			extraInitrd = /boot/initrd.keys.gz;
		};

		boot.initrd.luks.devices = {
		"root" = {
			device = "/dev/disk/by-uuid/fee1574b-e92f-4115-9932-9a49a07359ed";
			preLVM = true;
			keyFile = "/keyfile0.bin";
			allowDiscards = true;
		};
		};

# Data mount
		fileSystems."/data" = {
			device = "/dev/disk/by-uuid/f00b20fd-daaa-424a-af83-2722da19d773";
			encrypted = {
				enable = true;
				label = "crypted-data";
				blkDev = "/dev/disk/by-uuid/833196d8-c5a1-4ddc-82f1-d0493936e3f9";
				keyFile = "/keyfile1.bin";
			};
		};
}
