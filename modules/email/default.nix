{ config, lib, pkgs, ... }:
with lib; {
  options.alex.email.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure email accounts";
  };
  options.alex.email.useMutt = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to also configure mutt";
  };
  config = lib.mkIf config.alex.email.enable {
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

    programs.neomutt = lib.mkIf config.alex.email.useMutt {
      enable = true;
      extraConfig = lib.mkIf (config.alex.keyboardLayout == "dvp") ''
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
}
