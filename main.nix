{ pkgs, ... }: {
  nixpkgs.overlays = [ (import ./packages) ];
  nix.package = pkgs.nixUnstable;
  nixpkgs.config.allowUnfree = true;

  programs.zsh.enable = false;
  programs.fish.enable = true;

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
