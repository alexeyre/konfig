{
  boot.initrd.availableKernelModules =
    [ "xhci_pci" "ahci" "nvme" "usbhid" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" "thinkpad_acpi" ];
  boot.extraModulePackages = [ ];
  boot.extraModprobeConfig = ''
    options psmouse proto=imps
    options thinkpad_acpi force_load=1
  '';

}
