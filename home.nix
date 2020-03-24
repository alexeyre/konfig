{ config, lib, pkgs, ... }:
{
	imports = [
		./overlays.nix
		./qute.nix
	];
	services.xcape = {
		enable = true;
		mapExpression = { Control_L = "Escape"; };
	};
	services.compton = {
		enable = true;
		fade = true;
		fadeDelta = 2;
		inactiveDim = "0.2";
	};

	programs.mpv = {
		enable = true;
		profiles = {
			pip = {
				ontop = true;
				autofit = "384x216";
				geometry = "99%:2%";
			};
		};
		bindings = {
			"Ctrl+p" = "run bspc -t floating ; run bspc -g sticky=on ; apply-profile pip";
		};
	};
	programs.emacs.enable = true;

	home.packages = with pkgs; [
		kotatogram-desktop
		gopass
		kitty
		rofi
		qutebrowser
		vim

		# rust userutils
		cargo
		rustc
		bat
		exa
	];
	programs.urxvt = {
		enable = false;
		fonts = [ "xft:ProggyTinyTT:size=11:antialias=false:autohint=true" ];
		scroll.bar.enable = false;
	};
	services.random-background = {
		enable = true;
		imageDirectory = "%h/images/wallpapers";
	};
	home.stateVersion = "19.09";
	home.keyboard = {
		layout = "us";
		variant = "dvp";
		options = ["ctrl:nocaps"];
	};
	programs.newsboat = {
		enable = true;
		extraConfig = ''
macro y set browser "mpv %u"; open-in-browser ; set browser "elinks %u"
		'';
		urls = [
			{ tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"; } # lukesmithxyz
			{ tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCwgKmJM4ZJQRJ-U5NjvR2dg"; } # commaai archive
		];
	};
	xsession.enable = true;
	xsession.windowManager.bspwm = {
		enable = true;
		monitors = { eDP1 = [ "I" "II" "III" "IV" "V" ]; };
		settings = {
			top_padding = 22;
			focus_follows_pointer = true;
		};
		rules = {
			"Emacs".state = "tiled";
		};
		startupPrograms = [ "systemctl --user restart polybar" ];
	};
	services.polybar = {
		enable = true;
		config = {
			"bar/main" = {
				width = "100%";
				modules-right = "time";
				modules-left = "bspwm";
				height = "22";
				radius = 0;
			};
			"module/time" = {
				type = "internal/date";
				interval = 60;
				time = "%H:%M";
				label = "%time%";
			};
			"module/bspwm" = {
				type = "internal/bspwm";
				pin-workspaces = true;
				inline-mode = true;
				enable-click = false;
				fuzzy-match = true;
				label-focused-foreground = "\${colors.iopink}"; 
			};
		};
		extraConfig = ''
			[colors]
			bg        = #1b1d1e
			bg-alt    = #262829
			base0     = #1b1d1e
			base1     = #202020
			base2     = #303030
			base3     = #303030
			base4     = #505050
			base5     = #505050
			base6     = #808080
			base7     = #808080
			base8     = #DFDFDF
			fg        = #dddddd
			fg-alt    = #5B6268

			grey      = #505050
			red       = #d02b61
			orange    = #da8548
			green     = #60aa00
			teal      = #4db5bd
			yellow    = #d08928
			blue      = #6c9ef8
			dark-blue = #6688aa
			magenta   = #b77fdb
			violet    = #a9a1e1
			cyan      = #00aa80
			dark-cyan = #5699AF
			urlblue   = #57aadd
			iolime    = #bbfc20
			iopurple  = #bb20fc
			iocyan    = #20bbfc
			iopink    = #fc20bb
			ioteal    = #20fcbb
		'';
		script = "polybar main &";
	};
	services.sxhkd = {
		enable = true;
		keybindings = let
			bspc = "${pkgs.bspwm}/bin/bspc";
			launcher = "${pkgs.rofi}/bin/rofi -show run";
			term = "${pkgs.gnome3.gnome-terminal}/bin/gnome-terminal";
		in
		{
			"super + Return" = "${term}";
			"super + ampersand" = "${bspc} desktop -f I";
			"super + bracketleft" = "${bspc} desktop -f II";
			"super + braceleft" = "${bspc} desktop -f III";
			"super + braceright" = "${bspc} desktop -f IV";
			"super + parenleft" = "${bspc} desktop -f V";
			"super + p" = "${launcher}";
			"super + shift + c" = "${bspc} node -c";

			"super + shift + ampersand" = "${bspc} node -d I";
			"super + shift + bracketleft" = "${bspc} node -d II";
			"super + shift + braceleft" = "${bspc} node -d III";
			"super + shift + braceright" = "${bspc} node -d IV";
			"super + shift + parenleft" = "${bspc} node -d V";
		};
	};

}
