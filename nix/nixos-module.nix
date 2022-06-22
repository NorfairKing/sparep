{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
, sparepReleasePackages ? pkgs.sparepReleasePackages
, envname
}:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.sparep."${envname}";

  mergeListRecursively = pkgs.callPackage ./merge-lists-recursively.nix { };
in
{
  options.services.sparep."${envname}" =
    {
      enable = mkEnableOption "Sparep Service";
      api-server = mkOption {
        default = { };
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Sparep API Server";
            log-level = mkOption {
              type = types.str;
              example = "Debug";
              default = "Warn";
              description = "The log level to use";
            };
            hosts = mkOption {
              type = types.listOf (types.str);
              default = [ ];
              example = [ "api.sparep.cs-syd.eu" ];
              description = "The host to serve api requests on";
            };
            port = mkOption {
              type = types.int;
              example = 8001;
              description = "The port to serve api requests on";
            };
            local-backup = mkOption {
              type = types.nullOr (types.submodule {
                options = {
                  enable = mkEnableOption "Sparep API Server Local Backup Service";
                  backup-dir = mkOption {
                    type = types.str;
                    example = "backup/api-server";
                    default = "backup/api-server";
                    description = "The directory to store backups in, relative to the /www/sparep/${envname} directory or absolute";
                  };
                };
              });
              default = null;
            };
          };
        });
      };
      web-server = mkOption {
        default = { };
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Sparep Web Server";
            api-url = mkOption {
              type = types.str;
              example = "api.sparep.cs-syd.eu.eu";
              description = "The url for the api to use";
            };
            log-level = mkOption {
              type = types.str;
              example = "Debug";
              default = "Warn";
              description = "The log level to use";
            };
            hosts = mkOption {
              type = types.listOf (types.str);
              default = [ ];
              example = [ "sparep.cs-syd.eu" ];
              description = "The host to serve web requests on";
            };
            port = mkOption {
              type = types.int;
              example = 8002;
              description = "The port to serve web requests on";
            };
            google-analytics-tracking = mkOption {
              type = types.nullOr types.str;
              example = "XX-XXXXXXXX-XX";
              default = null;
              description = "The Google analytics tracking code";
            };
            google-search-console-verification = mkOption {
              type = types.nullOr types.str;
              example = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX";
              default = null;
              description = "The Google search console verification code";
            };
          };
        });
      };
    };
  config =
    let
      working-dir = "/www/sparep/${envname}/";
      # The docs server
      api-server-working-dir = working-dir + "api-server/";
      api-server-database-file = api-server-working-dir + "sparep-server-database.sqlite3";
      # The api server
      api-server-service =
        with cfg.api-server;
        optionalAttrs enable {
          "sparep-api-server-${envname}" = {
            description = "Sparep API Server ${envname} Service";
            wantedBy = [ "multi-user.target" ];
            environment =
              {
                "SPAREP_API_SERVER_LOG_LEVEL" =
                  "${builtins.toString log-level}";
                "SPAREP_API_SERVER_PORT" =
                  "${builtins.toString port}";
                "SPAREP_API_SERVER_DATABASE" = api-server-database-file;
              };
            script =
              ''
                mkdir -p "${api-server-working-dir}"
                cd ${api-server-working-dir};
                ${sparepReleasePackages.sparep-api-server}/bin/sparep-api-server
              '';
            serviceConfig =
              {
                Restart = "always";
                RestartSec = 1;
                Nice = 15;
              };
            unitConfig =
              {
                StartLimitIntervalSec = 0;
                # ensure Restart=always is always honoured
              };
          };
        };
      api-server-host =
        with cfg.api-server;

        optionalAttrs (enable && hosts != [ ]) {
          "${head hosts}" =
            {
              enableACME = true;
              forceSSL = true;
              locations."/" = {
                proxyPass = "http://localhost:${builtins.toString port}";
                # Just to make sure we don't run into 413 errors on big syncs
                extraConfig = ''
                  client_max_body_size 0;
                '';
              };
              serverAliases = tail hosts;
            };
        };

      # Local backup
      local-backup-service =
        optionalAttrs (cfg.api-server.enable or false) (
          optionalAttrs (cfg.api-server.local-backup.enable or false) (
            with cfg.api-server.local-backup;
            {
              "sparep-api-server-local-backup-${envname}" = {
                description = "Backup sparep-api-server database locally for ${envname}";
                wantedBy = [ ];
                script =
                  ''
                    mkdir -p ${backup-dir}
                    cd ${working-dir}
                    file="${backup-dir}/''$(date +%F_%T).db"
                    ${pkgs.sqlite}/bin/sqlite3 ${api-server-database-file} ".backup ''${file}"
                  '';
                serviceConfig = {
                  Type = "oneshot";
                };
              };
            }
          )
        );
      local-backup-timer =
        optionalAttrs (cfg.api-server.enable or false) (
          optionalAttrs (cfg.api-server.local-backup.enable or false) (
            with cfg.api-server.local-backup;
            {
              "sparep-api-server-local-backup-${envname}" = {
                description = "Backup sparep-api-server database locally for ${envname} every twelve hours.";
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "00/12:00";
                  Persistent = true;
                };
              };
            }
          )
        );

      # The web server
      web-server-working-dir = working-dir + "web-server/";
      web-server-data-dir = web-server-working-dir + "web-server/";
      web-server-service = optionalAttrs (cfg.web-server.enable or false) (with cfg.web-server; {
        "sparep-web-server-${envname}" = {
          description = "Sparep web server ${envname} Service";
          wantedBy = [ "multi-user.target" ];
          environment =
            {
              "SPAREP_WEB_SERVER_API_URL" = "${api-url}";
              "SPAREP_WEB_SERVER_LOG_LEVEL" = "${builtins.toString log-level}";
              "SPAREP_WEB_SERVER_PORT" = "${builtins.toString port}";
              "SPAREP_WEB_SERVER_DATA_DIR" = web-server-data-dir;
            } // optionalAttrs (!builtins.isNull google-analytics-tracking) {
              "SPAREP_WEB_SERVER_GOOGLE_ANALYTICS_TRACKING" = "${google-analytics-tracking}";
            } // optionalAttrs (!builtins.isNull google-search-console-verification) {
              "SPAREP_WEB_SERVER_GOOGLE_SEARCH_CONSOLE_VERIFICATION" = "${google-search-console-verification}";
            };
          script =
            ''
              mkdir -p "${web-server-working-dir}"
              cd ${web-server-working-dir}
              ${sparepReleasePackages.sparep-web-server}/bin/sparep-web-server serve
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
          unitConfig =
            {
              StartLimitIntervalSec = 0;
              # ensure Restart=always is always honoured
            };
        };
      });
      web-server-host = optionalAttrs ((cfg.web-server.enable or false) && cfg.web-server.hosts != [ ]) (with cfg.web-server; {
        "${head cfg.web-server.hosts}" =
          {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              proxyPass = "http://localhost:${builtins.toString port}";
              # Just to make sure we don't run into 413 errors on big syncs
              extraConfig = ''
                client_max_body_size 0;
              '';
            };
            serverAliases = tail hosts;
          };
      });
    in
    mkIf cfg.enable {
      systemd.services =
        mergeListRecursively [
          api-server-service
          web-server-service
          local-backup-service
        ];
      systemd.timers =
        mergeListRecursively [
          local-backup-timer
        ];
      networking.firewall.allowedTCPPorts = builtins.concatLists [
        (optional cfg.api-server.enable cfg.api-server.port)
        (optional cfg.web-server.enable cfg.web-server.port)
      ];
      services.nginx.virtualHosts =
        mergeListRecursively [
          api-server-host
          web-server-host
        ];
    };
}
