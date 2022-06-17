{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
, sparepReleasePackages ? pkgs.sparepReleasePackages
}:
let
  sparep-production = import (./nixos-module.nix) {
    inherit sources;
    inherit pkgs;
    inherit sparepReleasePackages;
    envname = "production";
  };
  home-manager = import (sources.home-manager + "/nixos/default.nix");

  api-port = 8001;
  web-port = 8002;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "sparep-module-test";
    machine = {
      imports = [
        sparep-production
        home-manager
      ];
      services.sparep.production = {
        enable = true;
        api-server = {
          enable = true;
          port = api-port;
        };
        web-server = {
          enable = true;
          port = web-port;
          api-url = "localhost:${builtins.toString api-port}";
        };
      };
      users.users.testuser.isNormalUser = true;
      home-manager.users.testuser = { pkgs, ... }: {
        imports = [
          ./home-manager-module.nix
        ];
        xdg.enable = true;
        programs.sparep = {
          enable = true;
          inherit sparepReleasePackages;
          completion-command = "echo 'hi'";
          sync = {
            enable = true;
            server-url = "localhost:${builtins.toString api-port}";
            username = "testuser";
            password = "testpassword";
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      machine.wait_for_unit("multi-user.target")

      machine.wait_for_open_port(${builtins.toString api-port})
      machine.succeed("curl localhost:${builtins.toString api-port}")
      machine.wait_for_open_port(${builtins.toString web-port})
      machine.succeed("curl localhost:${builtins.toString web-port}")

      machine.wait_for_unit("home-manager-testuser.service")

      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"

      machine.succeed(su("testuser", "cat ~/.config/sparep/config.yaml"))

      machine.succeed(su("testuser", "sparep register"))
      machine.succeed(su("testuser", "sparep login"))
      machine.succeed(su("testuser", "sparep sync"))
    '';
  }
)
