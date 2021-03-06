{ lib, modulesPath, ... }:

let
  ipAddress = "138.68.137.228";
  ipv6Address = "2a03:b0c0:1:d0::cfc:f001"; in {
imports = [
  (builtins.fetchTarball "https://github.com/nixcloud/nixcloud-webservices/archive/7e421fed1cb6dc460468d5917bb93b559606c7b6.tar.gz")
(modulesPath + "/profiles/qemu-guest.nix")
  ];
in {
  nixcloud.email= {
    enable = true;
    domains = [ "alexey.re" ];
    ipAddress = ipAddress;
    ip6Address = ipv6Address;
    fqdn = "mail.alexey.re";
    users = [
      # see https://wiki.dovecot.org/Authentication/PasswordSchemes
      { name = "a"; domain = "alexey.re"; password = "{SHA512-CRYPT}$6$643746f8428dddc271bf753089e199fc2f308104348f149f2b7a8db7a7c0dda3f27e25ae73c46dd171ecf2dc5b4e539201668af57d4263cecb9409abc92dd8d8"; }
    ];
  };

  boot.cleanTmpDir = true;
  networking.hostName = "landchad";
  networking.firewall.allowPing = true;
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDDXEFJysbsRZbZFtgKbdAr/Gagp2fI09hN5pGbLPQcdP5hUuuj5++ZjOXcf6XE001tZOFato7dIV7vYZqHpzgzFg9quqkhYABBmhS36XNFp6A7L/n80FkzIjnaYJSzErilipi2CCRJNevkfgVQCFozRX6MKUQzkkEAvZTuHoL/i8e03BNmnOsCOhv8QhdQeU2x6063iQUa+LUwQa5dSB0Hl+ZB5geHC7GphQ3P01E3UQ8H2nznklP3o95ksAM3Ophz0qPVOhQvq+PLw8zrFzIAkFBRm4uV6RFtOtQrHeOESpvGbbEYuiFawoDiZD1LF5kWYLJ5PQfR/9WfbcG6bEm74KvDvkhlAtyxVav/CUCWJDIzZf9rVQbSTlnuIIB2usk48w7RV+p/UvZ09rdNTadBBXx8udYm+Yl5f6kevS/qnGC4FketHOy3MPYqkZSki0U64jjzXky+sR+FrfNSkvlmj24oJQjXxA6Vh/NCAE1GodbehJJSyvLPfTUbnP60uh8= u0_a319@localhost" 
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCpl/im+IcMYfQLBaW6oFs+2ewPMXiSGgpj0XUojo3LndEoVZNsZX5EunSMLLMtsH6IxkbhjcIlfSB/tOk7VlXKrKRVOfa8J1d2ZWRJfVyvYhQdezIlEOG19hqTwuSQa77jAh7fup/foVq/4zV5dYFbJie8VG/ax7mQCXr6YtjmsmiehRCEH6cz9LXnZO8k+ILWipXELji15EAZzAAI6EyBH5goHIDPxZMyD/p0SIxnF3fxolSNLXFyNwxwNVSlqYmvaIqC6yijfjS8NCxDjO3JO7Gy4bZSsL5VYGB41Z6gEQpp0Z5iTFPvcS6+mk50ihgWeCHjF6VlfaBd3l3KyTff23CANrLaL5GvhcQ1ZXbRTvFrniB1hIKhHpBeK+xZ4Iyseb3MIwv8S/dR/BuB+yIJG2rX6IQcWR2QVUQL4WKnui3gyu6G/qZTGis4JmilVoAo8vOL9qbhrlBowahkOKeXbU6CoJvSHjAqxagh9aUXRip1wmfWHyEP5IXWfyHTJ2U= alex@Alexs-iMac.local" 
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDLKXwk9ocG/Y3K9+Cd4tR1+mr5CXV0VSgyZRMTNHA3VSn30ViemN3qk2GitdzvsVI+EfLLarJAoYITcYJj9/YMDmMxWw9Y7GYP6UPX4PNTnfezQ4os9VfejYAwlcvlQH0mA0HsRGTx+P6TK+ENGsero51z4+3L62JqXLbkc/kISqSZDcjSJ7HZMr+zw2ewrZvjmP2FuCIjD1tuuLKPprWvdiLN0CwURo/TXZecxzTiUmL3x6ocStzfLnzOjZkLmcChHLAb+xLFOFMCSGtmy/LIzwvzhNchwXpfWoFHVErKJYuTJJkxCi6x3v9U6XxqbwVZ3RHSrCF8fdsv18EGcBqmtpZv/qmtdqzs9rpgZ4MAMa7aYnS4yF9PETY9bWTNcIhQivI1cRmfuWPCiJkGt9GmMLpDniGZS9gcVbOOHmPHGQYvn4plZa+vr0oFTyUhOYQZzxKSwd0ptd9A0MIesyUC0T31GS0ta78oW0T8XHGxpT4ebeUgLrJIry0qYSYOo6tbsdc59tXZyqDCpgLLIJGp0P5ePK2YdR0YRcYz5Z97N89Ef+eSeOMqsMY7Yih+HVmau74buj8RQmQ9+zwhWerCv5D6kX9fQc/wcMZwXwqoPyycQYpUXih395Cpbia9IPiYlS79HaZc0cqbCR0AQyO/f5zKJJO7kq0lwdNN/mbQ6w== alexeeyre@gmail.com" 
  ];
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [ "8.8.8.8"
                  ];
    defaultGateway = "138.68.128.1";
    defaultGateway6 = "2a03:b0c0:1:d0::1";
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          { address="138.68.137.228"; prefixLength=20; }
          { address="10.16.0.7"; prefixLength=16; }
        ];
        ipv6.addresses = [
          { address="2a03:b0c0:1:d0::cfc:f001"; prefixLength=64; }
          { address="fe80::bcd0:4fff:fe91:ab4f"; prefixLength=64; }
        ];
        ipv4.routes = [ { address = "138.68.128.1"; prefixLength = 32; } ];
        ipv6.routes = [ { address = "2a03:b0c0:1:d0::1"; prefixLength = 32; } ];
      };
      
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="be:d0:4f:91:ab:4f", NAME="eth0"
    ATTR{address}=="3e:91:10:39:7b:3f", NAME="eth1"
  '';
  imports = [  ];
  boot.loader.grub.device = "/dev/vda";
  fileSystems."/" = { device = "/dev/vda1"; fsType = "ext4"; };
}
