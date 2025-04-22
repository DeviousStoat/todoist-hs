{
  description = "A Haskell project";

  inputs.hix.url = "github:tek/hix?ref=0.9.1";

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
        cabal.meta.synopsis = "A Haskell client for the Todoist API";

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
      };
    };
}
