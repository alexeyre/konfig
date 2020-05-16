{ config, pkgs, lib, ... }:

{
  imports = [ ./overlays.nix <home-manager/nixos> ];
  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" ]; })
    allTheIcons
    croscore
    noto-fonts-emoji
  ];
  fonts.fontconfig = {
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
  networking.extraHosts = let
    list = builtins.readFile (builtins.fetchurl
      "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts");
  in ''
    ${list}
    0.0.0.0 youtube.com
    0.0.0.0 twitter.com
      '';
  environment.systemPackages = with pkgs; [
    gitMinimal
    wireguard-tools
    plasma5.kde-gtk-config
  ];
  environment.pathsToLink = [ "${pkgs.xorg.libxcb}/lib/" "/share/zsh" ];

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "dvp";
    xkbOptions = "ctrl:nocaps, compose:altgr";
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
  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;
  home-manager.users.alex = (import ./home);
  system.autoUpgrade = {
    enable = false;
    allowReboot = false;
    channel = "https://nixos.org/channels/nixos-unstable";
  };
}
