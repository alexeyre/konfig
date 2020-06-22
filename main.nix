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
  programs.ssh.askPassword = "";
  services.udisks2.enable = true;
  virtualisation.docker.enable = false;
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ATTR{idVendor}=="05ac", MODE="0666", GROUP="wheel"
      '';
  networking.extraHosts = let
    list = builtins.readFile (builtins.fetchurl
      "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts");
    youtube = builtins.readFile (builtins.fetchurl "https://gist.githubusercontent.com/Ewpratten/a25ae63a7200c02c850fede2f32453cf/raw/b9318009399b99e822515d388b8458557d828c37/hosts-yt-ads");
  in ''
    ${list}
    ${youtube}
    172.217.1.14 manifest.googlevideo.com
    0.0.0.0 *.ignore.me
    0.0.0.0 www.ignore.me
    0.0.0.0 ignore.me

      '';
  environment.systemPackages = with pkgs; [ gitMinimal wireguard-tools ];
  environment.pathsToLink = [ "${pkgs.xorg.libxcb}/lib/" "/share/zsh" ];

  services.xserver = {
    enable = true;
    layout = "us";
    xkbVariant = "dvp";
    xkbOptions = "ctrl:nocaps, compose:ralt";
    libinput.enable = true;
    libinput.tapping = false;
    displayManager.xserverArgs = [ "-ardelay 300" "-arinterval 25" ];
    desktopManager = { xterm.enable = false; };
    windowManager.bspwm.enable = true;
    # windowManager.xmonad.enable = true;
    displayManager.lightdm.enable = true;
    displayManager.lightdm.background = ./media/wallpapers/lock.png;
    xautolock.enable = true;
    xautolock.locker =
      "${pkgs.i3lock-fancy}/bin/i3lock-fancy";
  };
  programs.xss-lock.enable = true;
  programs.xss-lock.lockerCommand = "${config.services.xserver.xautolock.locker}";

  programs.zsh = { enable = true; };
  users.users.alex = {
    isNormalUser = true;
    extraGroups =
      [ "wheel" "audio" "adbusers" "video" "networkmanager" "docker" "vboxusers" ];
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
