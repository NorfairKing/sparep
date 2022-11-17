{
  description = "sparep";
  nixConfig = {
    extra-substituters = "https://sparep.cachix.org";
    extra-trusted-public-keys = "sparep.cachix.org-1:TaKpFhyK9z3Ob9dHx8Tf5VRL0cZlHAloy9wC09Wp2r8=";
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    home-manager.url = "github:nix-community/home-manager?ref=release-22.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
    appendful.url = "github:NorfairKing/appendful?ref=flake";
    appendful.flake = false;
    cursor.url = "github:NorfairKing/cursor?ref=flake";
    cursor.flake = false;
    cursor-brick.url = "github:NorfairKing/cursor-brick?ref=flake";
    cursor-brick.flake = false;
    autorecorder.url = "github:NorfairKing/autorecorder?ref=flake";
    autorecorder.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , home-manager
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    , appendful
    , cursor
    , cursor-brick
    , autorecorder
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          self.overlays.${system}
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (appendful + "/nix/overlay.nix"))
          (import (validity + "/nix/overlay.nix"))
          (import (cursor + "/nix/overlay.nix"))
          (import (cursor-brick + "/nix/overlay.nix"))
          (import (autorecorder + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
      mkNixosModule = import ./nix/nixos-module.nix { inherit (pkgs.sparepReleasePackages) sparep-api-server sparep-web-server; };
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.sparepRelease;
      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        nixos-module-test = import ./nix/nixos-module-test.nix {
          inherit (pkgs) nixosTest;
          home-manager = home-manager.nixosModules.home-manager;
          sparep-nixos-module-factory = self.nixosModuleFactories.${system}.default;
          sparep-home-manager-module = self.homeManagerModules.${system}.default;
        };
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
          };
        };
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "sparep-shell";
        packages = p: builtins.attrValues p.sparepPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          niv
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nixosModules.${system}.default = mkNixosModule { envname = "production"; };
      nixosModuleFactories.${system}.default = mkNixosModule;
      homeManagerModules.${system}.default = import ./nix/home-manager-module.nix { sparepReleasePackages = pkgs.sparepReleasePackages; };
    };
}
