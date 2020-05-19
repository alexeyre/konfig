self: super: let cfg = super.config.programs.rofi; in {
  cfg.replaceDmenu = super.lib.mkOption {
    default = false;
    type = types.bool;
    description = ''
	    Replace dmenu in the system path?
	    '';
    config = cfg.config.overrideDerivation (old: rec {
		    mkIf (cfg.replaceDmenu) super.pkgs.writeScriptBin "dmenu" ''
		    #!${super.pkgs.stdenv.shell}
		    exec ${pkgs.rofi}/bin/rofi_dmenu "\$@"
		    '';
    });

  };
}
