final: previous:
with final.haskell.lib;

{
  sparepPackages =
    let
      sparepPkg =
        name:
          doBenchmark (
            addBuildDepend (
              failOnAllWarnings (
                disableLibraryProfiling (
                  final.haskellPackages.callCabal2nix name (final.gitignoreSource (../. + "/${name}")) {}
                )
              )
            ) (final.haskellPackages.autoexporter)
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
        "sparep-cli" = sparepPkgWithComp "sparep" "sparep-tui";
        "sparep-client" = sparepPkg "sparep-client";
        "sparep-client-data" = sparepPkg "sparep-client-data";
        "sparep-data" = sparepPkg "sparep-data";
        "sparep-data-gen" = sparepPkg "sparep-data-gen";
        "sparep-server-data" = sparepPkg "sparep-server-data";
        "sparep-web-server" = sparepPkgWithOwnComp "sparep-web-server";
      };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (
              old.overrides or (
                _:
                _:
                  {}
              )
            ) (
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
                      self.callCabal2nix "envparse" envparseRepo {}
                    );
                  cursorBrickRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "cursor-brick";
                      rev = "a7b47b03c8c5525234aaccc0c372e49a80134b9d";
                      sha256 = "sha256:1wk2sixf1ld48j6a14zgfadg41si6rl8gwmwdlkn0cqjiw9n7f4p";
                    };
                  cursorBrickPkg = self.callCabal2nix "cursor-brick" (cursorBrickRepo + "/cursor-brick") {};
                  appendfulRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "appendful";
                      rev = "fac6474856726fc507c42779955a75a12ef98cc7";
                      sha256 = "sha256:1vf3vlpcnnjmcbb27qazrzqp89bjrjjhiiksm09mcywrqlmpa2q0";
                    };
                  appendfulPkg = name: self.callCabal2nix "appendful" (appendfulRepo + "/${name}") {};
                in
                  final.sparepPackages // {
                    envparse = envparsePkg;
                    cursor-brick = cursorBrickPkg;
                    appendful = appendfulPkg "appendful";
                    appendful-persistent = appendfulPkg "appendful-persistent";
                    genvalidity-appendful = appendfulPkg "genvalidity-appendful";
                  }
            );
        }
    );
}
