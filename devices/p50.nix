# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
	hardware.bluetooth.enable = true;
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;
	boot.loader.efi.efiSysMountPoint = "/boot/efi";
	boot.loader.grub = {
		enable = true;
		device = "nodev";
		version = 2;
		efiSupport = true;
		enableCryptodisk = true;
		extraInitrd = "/boot/initrd.keys.gz";
	};
	networking.hostName = "memepad";
	hardware.bumblebee = {
		enable = true;
		connectDisplay = true;
		pmMethod = "bbswitch";
		driver = "nouveau";
	};
	imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix>];

	boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
	boot.initrd.kernelModules = [ "dm-snapshot" ];
	boot.kernelModules = [ "kvm-intel" ];
	boot.extraModulePackages = [ ];

	nix.maxJobs = lib.mkDefault 8;
	powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";


	fileSystems."/" =
	{ device = "/dev/disk/by-uuid/28cbad6c-02c5-4637-a26e-813e1357c826";
		fsType = "btrfs";
	};

	fileSystems."/boot/efi" =
	{ device = "/dev/disk/by-uuid/DE37-B09F";
		fsType = "vfat";
	};

	fileSystems."/data" =
	{ device = "/dev/disk/by-uuid/da208f59-017b-4f0d-a2ea-4fd260cb9bef";
		fsType = "btrfs";
		encrypted = {
			enable = true;
			label = "crypt-data";
			blkDev = "/dev/disk/by-uuid/427f80a3-0680-4bde-a559-7a7c86f514b1";
			keyFile = "/keyfile1.bin";
		};
	};

	boot.initrd.luks.devices = {
		"root" = {
			name = "root";
			device = "86c1bccf-5f34-4afe-aea3-2584ef11c3f8";
			preLVM = true;
			keyFile = "/keyfile0.bin";
			allowDiscards = true;
		};
	};

}
