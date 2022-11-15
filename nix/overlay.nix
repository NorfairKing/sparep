final: prev:

with final.lib;
with final.haskell.lib;

{
  sparepCasts =
    let
      mkCastDerivation = final.autorecorder.passthru.mkCastDerivationFunction
        { pkgs = final // final.sparepPackages; };
    in
    {
      sparep-basics-cast = mkCastDerivation {
        name = "sparep-basics-cast";
        src = ../casts/basics.yaml;
      };
    };

  sparepReleasePackages = mapAttrs
    (_: pkg: justStaticExecutables (doCheck pkg))
    final.haskellPackages.sparepPackages;

  sparepRelease =
    final.symlinkJoin {
      name = "sparep-release";
      paths = attrValues final.sparepReleasePackages;
    };

  haskellPackages =
    prev.haskellPackages.override (
      old:
      {
        overrides =
          composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                let
                  sparepPackages =
                    let
                      sparepPkg = name:
                        overrideCabal
                          (buildStrictly (self.callPackage (../${name}/default.nix) { }))
                          (old: {
                            doBenchmark = true;
                            doHaddock = false;
                            doCoverage = false;
                            doHoogle = false;
                            doCheck = false; # Only check the release version.
                            hyperlinkSource = false;
                            enableLibraryProfiling = false;
                            enableExecutableProfiling = false;

                            configureFlags = (old.configureFlags or [ ]) ++ [
                              # Optimisations
                              "--ghc-options=-O2"
                              # Extra warnings
                              "--ghc-options=-Wall"
                              "--ghc-options=-Wincomplete-uni-patterns"
                              "--ghc-options=-Wincomplete-record-updates"
                              "--ghc-options=-Wpartial-fields"
                              "--ghc-options=-Widentities"
                              "--ghc-options=-Wredundant-constraints"
                              "--ghc-options=-Wcpp-undef"
                              "--ghc-options=-Werror"
                            ];
                            buildDepends = (old.buildDepends or [ ]) ++ [
                              final.haskellPackages.autoexporter
                            ];
                            # Ugly hack because we can't just add flags to the 'test' invocation.
                            # Show test output as we go, instead of all at once afterwards.
                            testTarget = (old.testTarget or "") + " --show-details=direct";
                          });
                      sparepPkgWithComp =
                        exeName: name:
                        generateOptparseApplicativeCompletion exeName (sparepPkg name);
                      sparepPkgWithOwnComp = name: sparepPkgWithComp name name;

                    in
                    {
                      "sparep-api" = sparepPkg "sparep-api";
                      "sparep-api-gen" = sparepPkg "sparep-api-gen";
                      "sparep-api-server" = sparepPkgWithOwnComp "sparep-api-server";
                      "sparep-api-server-data" = sparepPkg "sparep-api-server-data";
                      "sparep-api-server-data-gen" = sparepPkg "sparep-api-server-data-gen";
                      "sparep-api-server-gen" = sparepPkg "sparep-api-server-gen";
                      "sparep-tui" = sparepPkgWithOwnComp "sparep-tui";
                      "sparep-cli" = sparepPkgWithComp "sparep" "sparep-cli";
                      "sparep-client" = sparepPkg "sparep-client";
                      "sparep-client-data" = sparepPkg "sparep-client-data";
                      "sparep-data" = sparepPkg "sparep-data";
                      "sparep-data-gen" = sparepPkg "sparep-data-gen";
                      "sparep-web-server" = sparepPkgWithOwnComp "sparep-web-server";
                    };
                in
                sparepPackages // {
                  inherit sparepPackages;
                }
            );
      }
    );
}
