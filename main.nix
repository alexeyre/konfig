{ config, pkgs, lib, mkDerivation, qtbase, qttools, qmake, ... }:

{
  nixpkgs.config.allowUnfree = true;
  imports = [ ./overlays.nix ];
  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
    allTheIcons
    croscore
    noto-fonts-emoji
  ];
  fonts.fontconfig = {
    penultimate.enable = false;
    defaultFonts = {
      serif = [ "Tinos" ];
      sansSerif = [ "Arimo" ];
      monospace = [ "FiraCode Nerd Font" ];
      emoji = [ "Noto Color Emoji" ];
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

  environment.variables.KDEWM = "${pkgs.bspwm}/bin/bspwm";
  virtualisation.docker.enable = true;
  services.tlp.enable = true;
  programs.slock.enable = true;
  networking.extraHosts = let
    list = builtins.readFile (builtins.fetchurl
      "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts");
  in ''
    ${list}
    0.0.0.0 youtube.com
    0.0.0.0 twitter.com
      '';
  environment.systemPackages = with pkgs; [
    home-manager-unstable.home-manager
    fwupd
    aircrack-ng
    git
    unzip
    gnupg
    p7zip
    wireguard-tools
    plasma5.kde-gtk-config
  ];
  environment.pathsToLink = [ "${pkgs.xorg.libxcb}/lib/" "/share/zsh" ];
  networking.firewall.enable = true;
  networking.firewall.checkReversePath = "loose";
  networking.firewall.allowedUDPPorts = [ 51820 ];
  networking.wireguard.enable = true;

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "dvp";
    xkbOptions = "ctrl:nocaps, compose:menu";
    libinput.enable = true;
    libinput.tapping = false;
    displayManager.xserverArgs = [ "-ardelay 300" "-arinterval 25" ];
    desktopManager = { xterm.enable = false; };
    displayManager.sddm.enable = true;
    windowManager.bspwm.enable = true;
    desktopManager.plasma5.enable = true;
  };
  programs.zsh = { enable = true; };
  users.users.alex = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "adbusers" "video" "networkmanager" ];
    shell = pkgs.zsh;
  };
  networking.networkmanager.enable = true;
  services.connman.enable = false;
  services.connman.extraConfig = ''
    [General]
    SingleConnectedTechnology=true
      '';
  networking.wireless = {
    userControlled.enable = true;
    networks = { TANHETOADETHOADETHAOD = { }; };
  };
  system.autoUpgrade = {
    enable = true;
    allowReboot = true;
    channel = "https://nixos.org/channels/nixos-unstable";
  };
}
