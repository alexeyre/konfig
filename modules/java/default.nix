{ config, lib, pkgs, ... }:
with lib;
let
  arm64jdk = lowPrio (pkgs.openjdk15_headless.overrideAttrs (old: rec {
    name = "zulu15.29.15-ca-jdk15.0.2";
    src = pkgs.fetchurl {
      url = "https://cdn.azul.com/zulu/bin/${name}-macosx_aarch64.tar.gz";
      sha256 = "1iggq5cq0gbqy41cjpchfd2mx61b529dl0ni1z6nwszgbvqw64r6";
      curlOpts = "-H Referer:https://www.azul.com/downloads/zulu/zulu-mac/";
    };
  }));
  arm64gradle = pkgs.gradle.overrideAttrs (old: rec {
    name = "gradle-7.0-20210212230754+0000";
    src = pkgs.fetchurl {
      url =
        "https://services.gradle.org/distributions-snapshots/${name}-bin.zip";
      sha256 = "0924b791qb8lh7rrvj51cnyc42j4xfsqnss2m6p2hxarfphqm4hs";
    };
    installPhase = ''
      mkdir -pv $out/lib/gradle/
      cp -rv lib/ $out/lib/gradle/
      gradle_launcher_jar=$(echo $out/lib/gradle/lib/gradle-launcher-*.jar)
      test -f $gradle_launcher_jar
      makeWrapper ${arm64jdk}/bin/java $out/bin/gradle \
      --set JAVA_HOME ${arm64jdk} \
      --add-flags "-classpath $gradle_launcher_jar org.gradle.launcher.GradleMain"
    '';
    buildInputs = with pkgs; [ unzip makeWrapper arm64jdk ];
  });
in {
  options.alex.java.enable = mkOption {
    type = types.bool;
    default = false;
    description = "Whether to configure JRE for java development";
  };

  options.alex.java.bloat = mkOption {
    type = types.bool;
    default = false;
    description =
      "Whether to configure a bloated, disgusting environment for disgusting, inf1b java development 🤮";
  };
  config = mkIf config.alex.java.enable {
    home.packages = lib.mkIf config.alex.java.enable
      (if config.alex.is-mac then [ arm64jdk arm64gradle ] else [ jdk ]);
    alex.brew.casks = lib.mkIf config.alex.java.bloat [ "jetbrains-toolbox" ];
  };
}
