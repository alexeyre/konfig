{ config, pkgs, lib, ... }:
{
	imports =
		[ # Include the results of the hardware scan.
		<nixpkgs/nixos/modules/installer/scan/not-detected.nix>
		../main.nix
		];

	boot.loader.grub.enable = true;
	boot.loader.grub.version = 2;
	boot.loader.grub.efiSupport = false;
	boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

	networking.hostName = "smallpad"; # Define your hostname.
	networking.useDHCP = false;
	networking.interfaces.enp0s25.useDHCP = false;
	networking.interfaces.wls1.useDHCP = false;
	sound.enable = true;
	boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" "usb_storage" "sd_mod" ];
	boot.initrd.kernelModules = [ ];
	boot.kernelModules = [ "kvm-intel" ];
	boot.extraModulePackages = [ ];
	fileSystems."/" =
	{ device = "/dev/sda3";
		fsType = "btrfs";
	};
	fileSystems."/boot" =
	{ device = "/dev/disk/by-uuid/5B8D-353E";
		fsType = "vfat";
	};
	swapDevices = [ ];
	nix.maxJobs = lib.mkDefault 2;
}

