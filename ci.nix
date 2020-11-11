let
  pkgs = import ./nix/pkgs.nix;
  nix-pre-commit-hooks =
    import (
      builtins.fetchTarball "https://github.com/hercules-ci/nix-pre-commit-hooks/archive/4dd50ef441796b439a56f1e0f8b127d4129f8947.tar.gz"
    );
in
{
  release = pkgs.sparepRelease;
  casts = pkgs.sparepCasts;
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      nixpkgs-fmt.enable = true;
      hlint.enable = true;
      ormolu.enable = true;
    };
  };
}
