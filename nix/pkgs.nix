let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay =
    import (
      pkgs.fetchFromGitHub (import ./validity-version.nix) + "/nix/overlay.nix"
    );
  cursor-overlay =
    import (
      pkgs.fetchFromGitHub (import ./cursor-version.nix) + "/nix/overlay.nix"
    );
  yamlparse-applicative-overlay =
    import (
      pkgs.fetchFromGitHub (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );
  appendful-overlay =
    import (
      pkgs.fetchFromGitHub (import ./appendful-version.nix) + "/nix/overlay.nix"
    );
  hastoryPkgs =
    pkgsv {
      overlays =
        [
          validity-overlay
          cursor-overlay
          yamlparse-applicative-overlay
          appendful-overlay
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
hastoryPkgs
