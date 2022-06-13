let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

in
{
  release = pkgs.sparepRelease;
  casts = pkgs.sparepCasts;
  nixos-module-test = import ./nix/nixos-module-test.nix { inherit pkgs; };
  pre-commit-check = pre-commit.run;
  shell = pkgs.symlinkJoin {
    name = "shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
}
