let
  pkgsv = import (import ../../nix/nixpkgs.nix);
  pkgs = pkgsv {};
  sparepFrontPkgs =
    pkgsv {
      overlays =
        [
          (import ../../nix/gitignore-src.nix)
          (import ./overlay.nix)
        ];
    };
in
sparepFrontPkgs
