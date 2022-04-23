{ config, lib, pkgs, ... }:
with lib; {
  imports = [ ./keyboard ./vi ./shell.nix ./yabai.nix ];
  networking.hostName = "fedbook";
  nix.trustedUsers = [ "alex" ];
  services.nix-daemon.enable = true;
  nixpkgs.system = "aarch64-darwin";
  nix.extraOptions = ''
    build-users-group = nixbld
    experimental-features = nix-command flakes
  '';

  environment.darwinConfig = "$HOME/.config/nix/configuration.nix";

  # ugly hack
  users.users.alex.home = "/Users/alex";

  # Nix tooling options
  nix.package = pkgs.nixUnstable; # Needed for nix flakes
  nixpkgs.config.allowUnfree = true; # Allow the installation of unfree packages

  # Enter home-configuration
  home-manager.users.alex = { ... }: {
    imports = [ ./brew ./alfred ];

    home.extraOutputsToInstall = [ "man" ];

    programs.brew.enable = true;
    programs.brew.taps = [
      "candid82/brew"
      "homebrew/bundle"
      "homebrew/cask"
      "homebrew/core"
      "homebrew/services"
      "homebrew/cask-fonts"
      "homebrew/cask-versions"
    ];

    programs.brew.casks = [
      "spotify"
      "anki"
      "discord"
      "transmission-nightly"
      "visual-studio-code"
      "pdf-expert-beta"
    ];

    programs.kitty = {
      enable = true;
      darwinLaunchOptions = [ "--single-instance" ];
      font = {
        size = 14;
        name = "monospace";
      };
      settings = {
        hide_window_decorations = true;
        sync_to_monitor = true;
        term = "xterm-256color";
        macos_quit_when_last_window_closed = true;
        resize_in_steps = true;
      };
    };

    # Enable the use of XDG directories, see https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
    xdg = {
      enable = true;
      cacheHome = ~/.local/cache;
    };

    ###########
    ### Git ###
    ###########
    programs.gh.enable = false; # Enable GitHub specific tooling
    programs.git.lfs.enable = true; # Enable Git LFS
    programs.git = {
      enable = true;
      userName = "Alex Eyre";
      userEmail = "alexeeyre@gmail.com";
      signing = {
        signByDefault = true;
        key = "954EBF489FD70E2E4694082B3E1F5A8C0C4F9FB3";
        gpgPath = "${pkgs.gnupg}/bin/gpg";
      };
      extraConfig.pull = {
        rebase = true;
        merge = false;
      };
    };

    programs.readline = {
      enable = true;
      variables."bell-style" = "none";
    };
  };
}
