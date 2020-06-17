{
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/1cc49fed-355d-4190-9cb3-cc63564050b6";
    fsType = "btrfs";
  };

  fileSystems."/boot/efi" = {
    device = "/dev/disk/by-uuid/3040-B4CD";
    fsType = "vfat";
  };
  swapDevices =
    [{ device = "/dev/disk/by-uuid/e71b529b-84fb-416e-b832-093ae522750f"; }];
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.grub = {
    enable = true;
    device = "nodev";
    version = 2;
    efiSupport = true;
    enableCryptodisk = true;
  };
  boot.initrd.secrets = {
    "/keyfile0.bin" = "/etc/secrets/initrd/keyfile0.bin";
    "/keyfile1.bin" = "/etc/secrets/initrd/keyfile1.bin";
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
