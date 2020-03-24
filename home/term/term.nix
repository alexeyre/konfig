{ config, lib, pkgs, ... }:

{
	imports = [ ./st.nix ];
	programs.kitty = {
		enable = false;
		font.package = pkgs.nerdfonts;
		font.name = "FiraCode Nerd Font";
		settings = {
			font_size = "14.0";
		};
	};
	programs.st.enable = true;
}
