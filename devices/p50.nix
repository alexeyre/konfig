# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:
let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in {
  imports = [ ../main.nix ];
  hardware.bluetooth.enable = true;

  # sound
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  }; # for bluetooth support
  sound.enable = true;

  # make everything use the same file dialogue
  xdg.portal = {
    enable = false;
    gtkUsePortal = true;
  };
  qt5 = {
    enable = true;
    style = "gtk2";
    platformTheme = "gtk2";
  };

  # networking
  networking.hostName = "memepad";
  networking.wireguard.enable = true;
  networking.firewall = {
    enable = true;
    allowedUDPPorts = [ 51820 ];
  };
  networking.networkmanager.enable = false;
  networking.wireless.userControlled.enable = true;
  networking.wireless.networks = { AEORAOHECOA = { }; };
  services.connman.enable = true;

  # control backlight
  programs.light.enable = true;

  # dedicated card controls
  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "nvme" "usb_storage" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelParams = [ ]; # "usbcore.autosuspend=-1" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # printing
  services.printing = {
    enable = false;
    drivers = [ pkgs.samsung-unified-linux-driver ];
  };

  boot.kernelPackages = pkgs.linuxPackages_latest;
  hardware.nvidiaOptimus.disable = true;
  hardware.nvidia.prime = {
    #offload.enable = true;
    #sync.enable = true;
    intelBusId = "PCI:0:02:0";
    nvidiaBusId = "PCI:1:00:0";
  };
  hardware.opengl.enable = true;
  hardware.nvidia.powerManagement.enable = false;
  hardware.nvidia.modesetting.enable = false;
  services.xserver.dpi = 96;
  services.xserver.videoDrivers = [ "intel" ];
  environment.systemPackages = [ nvidia-offload ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/1cc49fed-355d-4190-9cb3-cc63564050b6";
    fsType = "btrfs";
  };

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/3040-B4CD";
    fsType = "vfat";
  };
  hardware.enableRedistributableFirmware = true;

  swapDevices =
    [{ device = "/dev/disk/by-uuid/e71b529b-84fb-416e-b832-093ae522750f"; }];

  nix.maxJobs = lib.mkDefault 8;

  # power management
  services.tlp.enable = true;
  services.tlp.extraConfig = ''
    # Operation mode when no power supply can be detected: AC, BAT.
    TLP_DEFAULT_MODE=BAT
    # Operation mode select: 0=depend on power source, 1=always use TLP_DEFAULT_MODE
    TLP_PERSISTENT_DEFAULT=0
    SATA_LINKPWR_ON_BAT=max_performance
    RUNTIME_PM_DRIVER_BLACKLIST="nouveau nvidia"
      '';
  powerManagement.enable = true;

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
