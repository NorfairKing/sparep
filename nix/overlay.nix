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
        "sparep" = sparepPkgWithOwnComp "sparep";
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
                      self.callCabal2nix "envparse" (envparseRepo) {}
                    );
                in
                  final.sparepPackages // {
                    envparse = envparsePkg;
                  }
            );
        }
    );
}
