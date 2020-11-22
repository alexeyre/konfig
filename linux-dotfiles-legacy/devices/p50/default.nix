# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }: {
  imports = [ ../../main.nix ./disks.nix ./kernel.nix ];
  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  # sound
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull; # for bluetooth support
  };
  sound.enable = true;
  virtualisation.virtualbox.host.enable = true;

  programs.slock.enable = true;

  # make everything use the same file dialogue
  xdg.portal = {
    enable = false;
    gtkUsePortal = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };
  qt5 = {
    enable = true;
    style = "gtk2";
    platformTheme = "gtk2";
  };

  environment.profileRelativeEnvVars = {
    XDG_DATA_DIRS = [ "${pkgs.gsettings-desktop-schemas}/share" ];
  };

  # networking
  networking.hostName = "memepad";
  networking.wireguard.enable = true;
  networking.firewall = {
    enable = true;
    allowedUDPPorts = [ 51820 ];
    checkReversePath = "loose";
  };
  networking.networkmanager.enable = false;
  networking.wireless.userControlled.enable = true;
  networking.wireless.networks = {
    AEORAOHECOA = { };
  }; # make wpa_supplicant work
  services.connman.enable = true;

  # printing
  services.printing = {
    enable = false;
    drivers = [ pkgs.samsung-unified-linux-driver ];
  };

  boot.kernelPackages = pkgs.linuxPackages_latest;
  hardware.nvidiaOptimus.disable = true;
  hardware.nvidia.prime = {
    offload.enable = false;
    intelBusId = "PCI:0:02:0";
    nvidiaBusId = "PCI:1:00:0";
  };
  hardware.opengl.enable = true;
  hardware.opengl.driSupport32Bit = true;
  hardware.nvidia.powerManagement.enable = false;
  hardware.nvidia.modesetting.enable = false;
  services.xserver.dpi = 96;
  services.xserver.videoDrivers = [ "intel" ];

  hardware.enableRedistributableFirmware = true;

  nix.maxJobs = lib.mkDefault 8;

  # power management
  services.tlp.enable = true;
  services.tlp.extraConfig = ''
    # Operation mode when no power supply can be detected: AC, BAT.
    TLP_DEFAULT_MODE=BAT
    # Operation mode select: 0=depend on power source, 1=always use TLP_DEFAULT_MODE
    TLP_PERSISTENT_DEFAULT=1
    SATA_LINKPWR_ON_BAT=max_performance
    RUNTIME_PM_DRIVER_BLACKLIST="nouveau nvidia"
      '';
  powerManagement.enable = true;

}
