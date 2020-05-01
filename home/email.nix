{ pkgs, ... }:
{
  accounts.email.accounts.edinburgh = {
    address = "A.Eyre@ed.ac.uk";
    mbsync.enable = true;
    smtp.host = "smtp.office365.com";
    smtp.tls.useStartTls = true;
    smtp.port = 587;
    imap.host = "outlook.office365.com";
    userName = "s2031787@ed.ac.uk";
    primary = true;
    passwordCommand = let gopass = "${pkgs.gopass}/bin/gopass"; in "${gopass} show -o \"websites/www.ed.ac.uk/s2031787@ed.ac.uk\"";
  };

  accounts.email.accounts.alexeeyre = {
      address = "alexeeyre@gmail.com";
      mbsync.enable = true;
      smtp.host = "smtp.gmail.com";
    smtp.tls.useStartTls = true;
    smtp.port = 587;
    imap.host = "imap.gmail.com";
    userName = "alexeeyre@gmail.com";
    passwordCommand = let gopass = "${pkgs.gopass}/bin/gopass"; in "${gopass} show -o \"websites/gmail.com/alexeeyre@gmail.com\"";
  };
  programs.mbsync.enable = true;
}
