{ config, lib, pkgs, ... }:
{
	imports = [
		../overlays.nix
		./qute.nix
		./emacs.nix
		./term/term.nix
	];
	
	programs.git = {
		enable = true;
		userName = "Alex Eyre";
		userEmail = "alex@turm.pw";
	};
	services.udiskie.enable = true;
	gtk = {
		enable = true;
		theme.package = pkgs.arc-theme;
		theme.name = "Arc-Dark";
		iconTheme.package = pkgs.arc-icon-theme;
		iconTheme.name = "Arc";
	};
	services.xcape = {
		enable = true;
		mapExpression = { Control_L = "Escape"; };
	};
	services.picom = {
		enable = true;
		fade = false;
		fadeDelta = 2;
	};
	services.syncthing.enable = true;
	programs.zsh = {
		enable = true;
		enableAutosuggestions = true;
		defaultKeymap = "viins";
		dotDir = ".config/zsh";
		sessionVariables = { ZSH_TMUX_AUTOSTART="true"; };
		oh-my-zsh = {
			enable = true;
			theme = "agnoster";
			plugins = [ "git" "tmux" ];
		};
	};
	xresources.properties = {
		"*.font" = "FiraCode Nerd Font:pixelsize=18:antialias=true:autohint=true;";
	};
	home.packages = with pkgs; [
		discord-canary
		kotatogram-desktop
		gopass
		qutebrowser
		obs-studio
		zoom-us
		dmenu
		vim
		spotify
		teams

		tor-browser-bundle-bin
		kotatogram-desktop
		hexchat
		anki

		# rust userutils
		cargo
		rustc
		bat
		exa

		toggldesktop

		neovim

		acpi
		sxiv
		deluge
		zathura
	];
	services.random-background = {
		enable = true;
		imageDirectory = "%h/images/wallpapers";
	};
	home.stateVersion = "19.09";
	services.keybase.enable = true;
	home.keyboard = {
		layout = "us";
		variant = "dvp";
		options = ["ctrl:nocaps"];
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

	programs.newsboat = {
		enable = true;
		extraConfig = ''
			macro y set browser "mpv --profile=pip %u"; open-in-browser ; set browser "elinks %u"
			'';
		urls = [
		{ tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA"; } # lukesmithxyz
		{ tags = ["youtube"]; url = "https://www.youtube.com/feeds/videos.xml?channel_id=UCwgKmJM4ZJQRJ-U5NjvR2dg"; } # commaai archive
		];
	};
	xsession.enable = true;
	xsession.windowManager.bspwm = {
		enable = true;
		monitors = { eDP1 = [ "I" "II" "III" "IV" "V" "msg" ]; };
		rules = {
			"kotatogram-desktop" = {
				desktop = "msg";
				follow = true;
			};
			"hexchat" = {
				desktop = "msg";
				follow = true;
			};
			"Emacs".state = "tiled";
			"mpv".state = "floating";
		};
		settings = {
			top_padding = 22;
			focus_follows_pointer = true;
		};
		startupPrograms = let 
			hexchat = "${pkgs.hexchat}/bin/hexchat";
			tgram = "${pkgs.kotatogram-desktop}/bin/kotatogram-desktop";
		in [ 
			"systemctl --user restart polybar" 
			"${hexchat}"
			"${tgram}"
		];
	};
	services.polybar = {
		enable = true;
		config = {
			"bar/main" = {
				width = "100%";
				modules-right = "battery time";
				modules-left = "bspwm";
				height = "22";
				radius = 0;
				font-0 = "FiraCode Nerd Font:size=12";
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
			"module/battery" = {
				type = "internal/battery";
				battery = "BAT0";
				adapter = "AC";

				full-at = 99;
				label-charging-foreground = "\${colors.iolime}";
				label-charging-disforeground = "\${colors.red}";

				label-charging = "%percentage%%";
				label-discharging = "%percentage%%";
				format-charging = "<label-charging> <ramp-capacity>+";
				format-discharging = "<label-discharging> <ramp-capacity>-";

				ramp-capacity-0 = "";
				ramp-capacity-1 = "";
				ramp-capacity-2 = "";
				ramp-capacity-3 = "";
				ramp-capacity-4 = "";
				ramp-capacity-5 = "";
				ramp-capacity-6 = "";
				ramp-capacity-7 = "";
				ramp-capacity-8 = "";
				ramp-capacity-9 = "";
				ramp-capacity-10 = "";
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
	programs.tmux = {
		enable = true;
		keyMode = "vi";
		shortcut = "a";
		plugins = with pkgs; [ tmuxPlugins.sensible tmuxPlugins.yank ];
	};
	services.sxhkd = {
		enable = true;
		keybindings = let
			bspc = "${pkgs.bspwm}/bin/bspc";
			launcher = "${pkgs.dmenu}/bin/dmenu_run";
			term = "${config.home.sessionVariables.TERMINAL}";
		in
		{
			"super + Return" = "${term}";
			"super + ampersand" = "${bspc} desktop -f I";
			"super + bracketleft" = "${bspc} desktop -f II";
			"super + braceleft" = "${bspc} desktop -f III";
			"super + braceright" = "${bspc} desktop -f IV";
			"super + parenleft" = "${bspc} desktop -f V";
			"super + bracketright" = "${bspc} desktop -f msg";
			"super + p" = "${launcher}";
			"super + shift + c" = "${bspc} node -c";

			"super + shift + Return" = "${bspc} node -s next\.local";

			"super + shift + ampersand" = "${bspc} node -d I";
			"super + shift + bracketleft" = "${bspc} node -d II";
			"super + shift + braceleft" = "${bspc} node -d III";
			"super + shift + braceright" = "${bspc} node -d IV";
			"super + shift + parenleft" = "${bspc} node -d V";
			"super + shift + bracketright" = "${bspc} node -d msg";
			"super + shift + q" = "pkill -9 Xorg";
			
			"super + shift + f" = ''${bspc} node -t "\~fullscreen"'';
			
			"super + s" = "${bspc} node -g sticky -g locked";
		};
	};

}
