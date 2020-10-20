{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.sparep;


in
{
  options =
    {
      programs.sparep =
        {
          enable = mkEnableOption "Sparep cli and syncing";
          decks =
            mkOption {
              type = types.listOf types.str;
              example = [ "~/decks" ];
              default = [];
              description = "Where to find the decks to study";
            };
          extraConfig =
            mkOption {
              type = types.str;
              description = "Extra contents for the config file";
              default = "";
            };
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Sparep syncing";
                        server-url =
                          mkOption {
                            type = types.str;
                            example = "api.sparep.cs-syd.eu";
                            description = "The url of the sync server";
                          };
                        username =
                          mkOption {
                            type = types.str;
                            example = "syd";
                            description =
                              "The username to use when logging into the sync server";
                          };
                        password =
                          mkOption {
                            type = types.str;
                            example = "hunter12";
                            description =
                              "The password to use when logging into the sync server";
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      sparepPkgs = (import ./pkgs.nix).sparepPackages;
      configContents = cfg: ''
        
decks: ${builtins.toJSON cfg.decks}
${cfg.extraConfig}

      '';
      syncConfigContents = syncCfg:
        optionalString (syncCfg.enable or false) ''

server-url: "${cfg.sync.server-url}"
username: "${cfg.sync.username}"
password: "${cfg.sync.password}"

      '';


      syncSparepName = "sync-sparep";
      syncSparepService =
        {
          Unit =
            {
              Description = "Sync sparep";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "sync-sparep-service-ExecStart"
                  ''
                    exec ${sparepPkgs.sparep-cli}/bin/sparep sync
                  ''}";
              Type = "oneshot";
            };
        };
      syncSparepTimer =
        {
          Unit =
            {
              Description = "Sync sparep every five minutes for";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "*:0/5";
              Persistent = true;
              Unit = "${syncSparepName}.service";
            };
        };

      sparepConfigContents =
        concatStringsSep "\n" [
          (configContents cfg)
          (syncConfigContents cfg.sync)
        ];

      services =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncSparepName}" = syncSparepService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncSparepName}" = syncSparepTimer;
          }
        );
      packages =
        [
          sparepPkgs.sparep-cli
          sparepPkgs.sparep-tui
        ];


    in
      mkIf cfg.enable {
        xdg = {
          configFile."sparep/config.yaml".text = sparepConfigContents;
        };
        systemd.user =
          {
            startServices = true;
            services = services;
            timers = timers;
          };
        home.packages = packages;
      };
}
