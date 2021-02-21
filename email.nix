{ pkgs, ... }: {
  programs.mbsync.enable = true;
  programs.mu.enable = true;
  programs.neomutt.enable = true;
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
    neomutt.enable = true;
    mbsync = {
      enable = true;
      create = "maildir";
    };
  };
}
