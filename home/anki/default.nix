self: super: {
  anki-latest = (super.pkgs.anki.overrideAttrs (old: rec {
    version = "2.1.26";
    src = super.fetchFromGitHub {
      owner = "ankitects";
      repo = "anki";
      rev = "2.1.26";
      sha256 = "12dvyf3j9df4nrhhnqbzd9b21rpzkh4i6yhhangn2zf7ch0pclss";
    };
    patches = [ ./no-version-check.patch ];
  }));
}
