{ config, lib, pkgs, ... }:
with lib; {
  imports = [
    ../options.nix # setup system-wide options
    ./keyboard.nix # supplies the current keymap for easier keyboard switching if desired
    ./shell.nix # set up the shell environment
    ./vi # Set up (n)vi(m)
  ];

  # Nix tooling options
  nix.package = pkgs.nixUnstable; # Needed for nix flakes
  nixpkgs.config.allowUnfree = true; # Allow the installation of unfree packages

  # Set up remote builder
  nix.buildMachines = [{
    hostName = "builder";
    system = "x86_64-linux";
    maxJobs = 1;
    speedFactor = 2;
    supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
    mandatoryFeatures = [ ];
  }];
  nix.distributedBuilds = true;
  nix.extraOptions = ''
    builders-use-substitutes = true
  '';

  # Enter home-configuration
  home-manager.users."${config.main-user}" = { ... }: {
    # Enable the use of XDG directories, see https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
    xdg.enable = true;

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

    home.packages = with pkgs; [
      gopass
      (writeScriptBin "strip_exif" ''
        #!${pkgs.stdenv.shell}
        ${pkgs.exiftool}/bin/exiftool -all= "$@"
      '')
      less
      (writeScriptBin "youtube-dl_wav" ''
        ${pkgs.youtube-dl}/bin/youtube-dl --ffmpeg-location=${pkgs.ffmpeg}/bin/ffmpeg -x --audio-format=wav $1
      '')
      (writeScriptBin "download_wallpaper" ''
        ${pkgs.curl}/bin/curl -l $1 -o ~/Pictures/papes/$(${pkgs.coreutils}/bin/basename $1)
      '')
      # (texlive.combine { inherit (texlive) plantuml minted fvextra scheme-full wrapfig ulem amsmath capt-of hyperref; })
      (pkgs.writeScriptBin "compress_video" ''
        #!${pkgs.stdenv.shell}
            file_name="''${1##*/}"
            file_name="''${file_name%.*}"
            file_name="''${file_name}.mp4"
            dir_name="$(dirname $1)"
            full_path="''${dir_name}/''${file_name}"
            test "$(file -i $1 | grep video)" = "video" && echo ${pkgs.ffmpeg} -i $1 -vcodec libx265 -crf 28 -tag:v hvc1 "''${full_path}"
          '')
    ];

    #############
    ### Email ###
    #############

    programs.mbsync.enable = true; # Enable MBSync to grab email from the server
    programs.mu.enable = true; # Enable the mu email indexing program
    accounts.email.maildirBasePath =
      ".local/share/maildir"; # Force the use of an XDG directory

    # Set up UoE email
    accounts.email.accounts.edinburgh = {
      # Account settings
      primary = true;
      realName = "Alex Eyre";
      address = "s2031787@ed.ac.uk";
      userName = "s2031787@ed.ac.uk";
      aliases = [ "A.Eyre@sms.ed.ac.uk" ];

      # Server settings
      imap = { host = "outlook.office365.com"; };
      smtp = {
        host = "smtp.office365.com";
        tls.useStartTls = true;
      };

      # Account access
      passwordCommand =
        "${pkgs.gopass}/bin/gopass show -o websites/www.ed.ac.uk/s2031787@ed.ac.uk";

      # Configure mu and msmtp
      mu.enable = true;
      msmtp.enable = true;

      # Enable creation of MBSync config
      mbsync = {
        enable = true;
        create = "maildir";
      };

      neomutt.enable = true;
    };

    # Set up the NeoMutt email reader
    programs.neomutt = {
      enable = true;
      # Shamelessly stolen (I think) and modified from https://github.com/Lukesmithxyz/voidrice
      extraConfig = mkIf (config.system.keyboard.layout == "dvp") ''
        #------------------------------------------------------------
        # Vi Key Bindings
        #------------------------------------------------------------

        # Moving around
          bind attach,browser,index       i   noop
          bind attach,browser,index       ii  first-entry
          bind attach,browser,index       I   last-entry
          bind pager                      i  noop
          bind pager                      i  top
          bind pager                      I   bottom
          bind pager                      t   previous-line
          bind pager                      h   next-line

        # Scrolling
          bind attach,browser,pager,index \CU next-page
          bind attach,browser,pager,index \CX previous-page
          bind attach,browser,pager,index \Cg half-up
          bind attach,browser,pager,index \Ce half-down
          bind browser,pager              \C\056 next-line
          bind browser,pager              \Cf previous-line
          bind index                      \C\056 next-line
          bind index                      \Cf previous-line

          bind pager,index                e   noop
          bind pager,index                ee  delete-message

        # Mail & Reply
          bind index                      \Cm list-reply # Doesn't work currently

        # Threads
          bind browser,pager,index        N   search-opposite
          bind pager,index                eY  delete-thread
          bind pager,index                ey  delete-subthread
          bind pager,index                iy  next-thread
          bind pager,index                iY  previous-thread
          bind index                      \047a  collapse-thread
          bind index                      \047A  collapse-all # Missing :folddisable/foldenable
      '';
      # TODO: add qwerty config
    };

    home.file.lessKeyConfig = mkIf (config.system.keyboard.layout == "dvp") {
      text = ''
        #command
        d left-scroll
        h forw-line
        t back-line
        n right-scroll
      '';
      target = ".lesskey";
      onChange = "${pkgs.less}/bin/lesskey";
    };

    ############
    ### tmux ###
    ############
    programs.tmux = {
      enable = true;
      keyMode = "vi"; # Use vi bindings inside tmux
      newSession = true; # Create a new session by default
      prefix = "C-a"; # Set the prefix to something that doesn't collide with vi
      escapeTime = 0; # Disable built-in delay on prefix key
      disableConfirmationPrompt =
        true; # Don't ask for confirmation when killing a pane
      terminal = "screen-256color"; # Use 256colors
      plugins = with pkgs.tmuxPlugins;
        [
          prefix-highlight # Adds a small indicator at the bottom when the prefix key is pressed
        ];

      extraConfig = ''
        bind c new-window -c "#{pane_current_path}"
        bind '"' split-window -c "#{pane_current_path}"
        bind % split-window -h -c "#{pane_current_path}"
      '' +
        # Configure dvorak select-pane bindings
        optionalString (config.system.keyboard.layout == "dvp") ''
          unbind-key j
          bind-key t select-pane -D
          unbind-key k
          bind-key n select-pane -U
          unbind-key h
          bind-key h select-pane -L
          unbind-key l
          bind-key s select-pane -R
        '';
    };
  };
}
