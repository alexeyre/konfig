{ config, lib, pkgs, ... }:
with lib; {

  imports = [ ../options.nix ];
  nix.package = pkgs.nixUnstable;
  nixpkgs.config.allowUnfree = true;

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
  main-user = { ... }: {
    xdg.enable = true;
    home.packages = with pkgs; [ niv gopass nixfmt ];
    programs.gh.enable = true;
    programs.git.lfs.enable = true;
    programs.readline = {
      enable = true;
      variables."bell-style" = "none";
    };

    home.packages = with pkgs; [
      fasd
      fzf
      (writeScriptBin { nativeBuildInputs = with pkgs; [ exiftool ]; }
        "strip_exif" ''
          #!${pkgs.stdenv.shell}
          exiftool -all= "$@"
        '')
      less
      (writeScriptBin "youtube-dl_wav" ''
        ${pkgs.youtube-dl}/bin/youtube-dl --ffmpeg-location=${pkgs.ffmpeg}/bin/ffmpeg -x --audio-format=wav $1
      '')
      (writeScriptBin "download_wallpaper" ''
        ${pkgs.curl}/bin/curl -l $1 -o ~/Pictures/papes/$(${pkgs.coreutils}/bin/basename $1)
      '')
      (texlive.combine {
        inherit (texlive)
          plantuml minted fvextra scheme-full wrapfig ulem amsmath capt-of
          hyperref;
      })
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

    # email
    programs.mbsync.enable = true;
    programs.mu.enable = true;
    accounts.email.maildirBasePath = ".local/share/maildir";
    accounts.email.accounts.edinburgh = {
      primary = true;
      realName = "Alex Eyre";
      address = "s2031787@ed.ac.uk";
      userName = "s2031787@ed.ac.uk";
      aliases = [ "A.Eyre@sms.ed.ac.uk" ];
      imap = { host = "outlook.office365.com"; };
      smtp = {
        host = "smtp.office365.com";
        tls.useStartTls = true;
      };

      passwordCommand =
        "${pkgs.gopass}/bin/gopass show -o websites/www.ed.ac.uk/s2031787@ed.ac.uk";

      mu.enable = true;
      msmtp.enable = true;

      mbsync = {
        enable = true;
        create = "maildir";
      };
    };

    programs.neomutt = {
      enable = true;
      extraConfig = ''
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
    };
  };
  programs.zsh.initExtra = ''
    source ${config.alex.fzf.directory}/shell/key-bindings.zsh
    bindkey -r "^T"
    bindkey '^O' fzf-file-widget
  '';
  programs.gh.gitProtocol = "ssh";
  programs.git = {
    enable = true;
    userName = "Alex Eyre";
    userEmail = "alexeeyre@gmail.com";
  };
  home.file.lessKeyConfig = {
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
  programs.tmux.tmuxp.enable = true;
  programs.tmux = {
    enable = true;
    keyMode = "vi";
    newSession = true;
    prefix = "C-a";
    escapeTime = 0;
    disableConfirmationPrompt = true;
    terminal = "screen-256color";
    plugins = with pkgs.tmuxPlugins; [ nord prefix-highlight ];
    extraConfig = ''
      setw -g window-status-current-format ' #I:#W#F '
      setw -g window-status-format ' #I:#W#F '
      setw -g mode-keys vi
      unbind-key j
      bind-key t select-pane -D
      unbind-key k
      bind-key n select-pane -U
      unbind-key h
      bind-key h select-pane -L
      unbind-key l
      bind-key s select-pane -R
      bind c new-window -c "#{pane_current_path}"
      bind '"' split-window -c "#{pane_current_path}"
      bind % split-window -h -c "#{pane_current_path}"
    '';
  };
}
