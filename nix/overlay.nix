final: previous:
with final.haskell.lib;

{
  sparepCasts =
    let
      mkCastDerivation = import
        (
          builtins.fetchGit
            {
              url = "https://github.com/NorfairKing/autorecorder";
              rev = "da5bf9d61108a4a89addc8203b1579a364ce8c01";
              ref = "master";
            } + "/nix/cast.nix"
        )
        { pkgs = final // final.sparepPackages; };
    in
    {
      sparep-basics-cast = mkCastDerivation {
        name = "sparep-basics-cast";
        src = ../casts/basics.yaml;
      };
    };
  sparepPackages =
    let
      sparepPkg =
        name:
        doBenchmark (
          addBuildDepend
            (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) { }
                )
              )
            )
            (final.haskellPackages.autoexporter)
        );
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

  sparepRelease =
    final.symlinkJoin {
      name = "sparep-release";
      paths = final.lib.attrValues final.sparepPackages;
    };

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
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
                  # envparse
                  envparseRepo =
                    final.fetchFromGitHub {
                      owner = "supki";
                      repo = "envparse";
                      rev = "de5944fb09e9d941fafa35c0f05446af348e7b4d";
                      sha256 =
                        "sha256:0piljyzplj3bjylnxqfl4zpc3vc88i9fjhsj06bk7xj48dv3jg3b";
                    };
                  envparsePkg =
                    dontCheck (
                      self.callCabal2nix "envparse" envparseRepo { }
                    );

                in
                final.sparepPackages // {
                  envparse = envparsePkg;
                }
            );
      }
    );
}
