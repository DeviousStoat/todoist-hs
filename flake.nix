{
  description = "A Haskell project";

  inputs.hix.url = "github:tek/hix?ref=0.8.0";

  outputs =
    { hix, ... }:
    hix.lib.flake {
      hackage.versionFile = "ops/version.nix";

      cabal = {
        license = "BSD-2-Clause-Patent";
        license-file = "LICENSE";
        author = "DeviousStoat";
        ghc-options = [ "-Wall" ];
        default-extensions = [
          "DataKinds"
          "DerivingStrategies"
          "DuplicateRecordFields"
          "OverloadedRecordDot"
          "OverloadedStrings"
        ];
      };

      packages.todoist-hs = {
        src = ./.;
        cabal.meta.synopsis = "A Haskell project";

        library = {
          enable = true;
          dependencies = [
            "aeson"
            "http-client"
            "http-client-tls"
            "servant"
            "servant-client"
            "text"
            "transformers"
          ];
        };

        executable.enable = true;

        test = {
          enable = true;
          dependencies = [
            "hedgehog >= 1.1 && < 1.5"
            "tasty ^>= 1.4"
            "tasty-hedgehog >= 1.3 && < 1.5"
          ];
        };

      };
    };
}
