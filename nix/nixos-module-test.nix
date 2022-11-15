{ nixosTest
, home-manager
, sparep-nixos-module-factory
, sparep-home-manager-module
}:
let
  sparep-production = sparep-nixos-module-factory {
    envname = "production";
  };

  api-port = 8001;
  web-port = 8002;
in
nixosTest ({ lib, pkgs, ... }: {
  name = "sparep-module-test";
  nodes = {
    apiserver = {
      imports = [
        sparep-production
      ];
      services.sparep.production = {
        enable = true;
        api-server = {
          enable = true;
          port = api-port;
        };
      };
    };
    webserver = {
      imports = [
        sparep-production
      ];
      services.sparep.production = {
        enable = true;
        web-server = {
          enable = true;
          port = web-port;
          api-url = "apiserver:${builtins.toString api-port}";
        };
      };
    };
    client = {
      imports = [
        home-manager
      ];
      users.users.testuser.isNormalUser = true;
      home-manager = {
        useGlobalPkgs = true;
        users.testuser = { pkgs, ... }: {
          imports = [
            sparep-home-manager-module
          ];
          xdg.enable = true;
          systemd.user.startServices = true;
          programs.sparep = {
            enable = true;
            completion-command = "echo 'hi'";
            sync = {
              enable = true;
              server-url = "apiserver:${builtins.toString api-port}";
              username = "testuser";
              password = "testpassword";
            };
          };
        };
      };
    };
  };
  testScript = ''
    from shlex import quote

    apiserver.start()
    webserver.start()
    client.start()

    apiserver.wait_for_unit("multi-user.target")
    webserver.wait_for_unit("multi-user.target")
    client.wait_for_unit("multi-user.target")

    apiserver.wait_for_open_port(${builtins.toString api-port})
    client.succeed("curl apiserver:${builtins.toString api-port}")

    webserver.wait_for_open_port(${builtins.toString web-port})
    client.succeed("curl webserver:${builtins.toString web-port}")

    client.wait_for_unit("home-manager-testuser.service")

    def su(user, cmd):
        return f"su - {user} -c {quote(cmd)}"

    client.succeed(su("testuser", "cat ~/.config/sparep/config.yaml"))

    client.succeed(su("testuser", "sparep register"))
    client.succeed(su("testuser", "sparep login"))
    client.succeed(su("testuser", "sparep sync"))
  '';
}
)
