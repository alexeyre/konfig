{ config, lib, pkgs, ... }: {
  imports = [ ./options.nix ];
  nixpkgs.overlays = [ (import ./packages) ];
  nix.package = pkgs.nixUnstable;
  nixpkgs.config.allowUnfree = true;

  programs.zsh.enable = false;
  programs.fish.enable = true;

  main-user = { ... }: {
    imports = [ ./modules ];
    xdg.enable = true;
    alex.vi.enable = true;
    alex.chromium.enable = false;
    alex.shell.enable = true;
    alex.git.enable = true;
    home.packages = with pkgs; [ niv gopass nixfmt ];
    programs.gh.enable = true;
    programs.git.lfs.enable = true;
    alex.newsboat.enable = false;
    alex.fzf.enable = true;
    alex.tex.enable = false;
    alex.fasd.enable = true;
    alex.less.enable = true;

    alex.games.enable = true;
    alex.games.battleNet = true;

    programs.readline = {
      enable = true;
      variables."bell-style" = "none";
    };
  };

  nix.buildMachines = [{
    hostName = "builder";
    system = "x86_64-linux";
    maxJobs = 1;
    speedFactor = 2;
    supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    mandatoryFeatures = [ ];
  }];
  nix.distributedBuilds = true;
  # optional, useful when the builder has a faster internet connection than yours
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';
}
