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
		initExtra = ''
			source <(antibody init)
			MODE_CURSOR_VICMD="#fc20bb block"
			MODE_CURSOR_VIINS="#fc20bb blinking bar"
			MODE_CURSOR_SEARCH="#fc20bb steady underline"
			antibody bundle softmoth/zsh-vim-mode
		'';
		dotDir = ".config/zsh";
		sessionVariables = { ZSH_TMUX_AUTOSTART="true"; };
		oh-my-zsh = {
			enable = true;
			theme = "agnoster";
			plugins = [ "git" "tmux" "vi-mode" ];
		};
	};
	home.packages = with pkgs; [
		light
		discord-canary
		kotatogram-desktop
		antibody
		gopass
		rofi
		qutebrowser
		vim
		spotify
		nodejs

		tor-browser-bundle-bin
		kotatogram-desktop
		hexchat
		anki

		calibre

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
		imageDirectory = "%h/images/4chan/wg";
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
		monitors = { 
			eDP1 = [ "I" "II" "III" "IV" "V" "msg" ];
			LVDS1 = [ "I" "II" "III" "IV" "V" "msg" ]; };
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
		in [ ];
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
				font-0 = "FiraCode Nerd Font:size=12;1";
				font-1 = "Noto Color Emoji:scale=10:style=Regular;2";
				module-margin-left = 0;
				module-margin-right = 1;
				overline-size = 1;
				overline-color = "\${colors.iopink}";
				line-size = 1;
				line-color = "\${colors.red}";
			};
			"module/time" = {
				type = "internal/date";
				interval = 60;
				time = "%H:%M";
				label = "%time%";
				label-foreground = "\${colors.green}";
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
				label-discharging-foreground = "\${colors.red}";

				label-charging = "%percentage%%";
				label-discharging = "%percentage%%";
				format-charging = "<label-charging> <animation-charging>";
				format-discharging = "<label-discharging> <ramp-capacity>";

				animation-charging-0 = "";
				animation-charging-1 = "";
				animation-charging-2 = "";
				animation-charging-3 = "";
				animation-charging-4 = "";
				animation-charging-5 = "";
				animation-charging-6 = "";
				animation-charging-7 = "";
				animation-charging-8 = "";
				animation-charging-9 = "";
				animation-charging-framerate = 250;

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
			launcher = "${pkgs.rofi}/bin/rofi -show run";
			term = "${pkgs.st}/bin/st";
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
