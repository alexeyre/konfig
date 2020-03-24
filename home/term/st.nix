{ config, pkgs, lib, ... }:
let
	cfg = config.programs.st;
in {
	options = {
		programs.st = {
			enable = lib.mkEnableOption "The suckless terminal emulator";
		};
	};
	config = lib.mkIf cfg.enable {
		home.packages = [ pkgs.st ];
		home.sessionVariables = { TERMINAL = "${pkgs.st}"; };
	};
}
