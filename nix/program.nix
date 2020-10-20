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
          workflowDir =
            mkOption {
              type = types.str;
              description = "Sparep' workflow directory";
              default = config.home.homeDirectory + "/workflow";
            };
          extraConfig =
            mkOption {
              type = types.str;
              description = "Extra contents for the config file";
              default = "";
            };
          backup =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Sparep backups";
                        backupDir =
                          mkOption {
                            type = types.str;
                            default = "${config.xdg.dataHome}/sparep/backup";
                            description = "The directory to backup to";
                          };
                      };
                  }
                );
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
          calendar =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Sparep calendar importing";
                        sources =
                          mkOption {
                            description = "The list of sources to import from";
                            type = types.listOf (
                              types.submodule {
                                options = {
                                  name =
                                    mkOption {
                                      type = types.nullOr types.str;
                                      default = null;
                                      example = "Personal";
                                      description = "The name of the source";
                                    };
                                  destination =
                                    mkOption {
                                      type = types.str;
                                      default = null;
                                      example = "calendar/name.sparep";
                                      description = "The destination file within the workflow directory";
                                    };
                                  source =
                                    mkOption {
                                      type = types.str;
                                      default = null;
                                      example = "https://calendar.google.com/calendar/ical/xxx.xxxxxxxxx%40gmail.com/private-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx/basic.ics";
                                      description = "The url to download the calendar from";
                                    };
                                };
                              }
                            );
                          };
                      };
                  }
                );
            };
          scheduler =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Sparep scheduler activation";
                        schedule =
                          mkOption {
                            description = "The schedule to activate";
                            type = types.listOf (
                              types.submodule {
                                options = {
                                  description =
                                    mkOption {
                                      type = types.nullOr types.str;
                                      default = null;
                                      example = "Weekly tasks for work";
                                      description = "A description of the schedule item. This is only used for logging and error messages.";
                                    };
                                  template =
                                    mkOption {
                                      type = types.nullOr types.str;
                                      default = null;
                                      example = "templates/weekly.sparep";
                                      description = "The relative path to the template in the workflow dir";
                                    };
                                  destination =
                                    mkOption {
                                      type = types.str;
                                      default = null;
                                      example = "workflow/work-[ %Y-%V | monday ].sparep";
                                      description = "The template relative path to the destination in the workflow dir";
                                    };
                                  schedule =
                                    mkOption {
                                      type = types.str;
                                      default = null;
                                      example = "0 12 * * 6"; # At 12:00 on saturday
                                      description = "The cron schedule for when to activate this item";
                                    };
                                };
                              }
                            );
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      sparepPkgs = (import ./project.nix {}).sparepPackages;
      backupSparepName = "backup-sparep";
      backupSparepService =
        {
          Unit =
            {
              Description = "Backup sparep";
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "backup-sparep-service-ExecStart"
                  ''
                    export PATH="$PATH:${pkgs.coreutils}/bin:${pkgs.gnutar}/bin:${pkgs.gzip}/bin"
                    set -ex
                    backupdir="${cfg.backup.backupDir}"
                    mkdir -p "''${backupdir}"
                    backupfile="''${backupdir}/''$(date +%F_%T).tar.gz"
                    tar -cvzf "''${backupfile}" "${cfg.workflowDir}"
                  ''}";
              Type = "oneshot";
            };
        };
      backupSparepTimer =
        {
          Unit =
            {
              Description = "Backup sparep every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "*-*-* 00:00";
              Persistent = true;
              Unit = "${backupSparepName}.service";
            };
        };

      syncConfigContents =
        syncCfg:
          optionalString (syncCfg.enable or false) ''

sync:
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
                    exec ${sparepPkgs.sparep-sync-client}/bin/sparep-sync-client sync
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

      calendarConfigContents =
        calendarCfg:
          optionalString (calendarCfg.enable or false) ''

calendar: ${builtins.toJSON calendarCfg}

    '';


      calendarSparepName = "calendar-sparep";
      calendarSparepService =
        {
          Unit =
            {
              Description = "Calendar import sparep";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "calendar-sparep-service-ExecStart"
                  ''
                    exec ${sparepPkgs.sparep-calendar-import}/bin/sparep-calendar-import
                  ''}";
              Type = "oneshot";
            };
        };
      calendarSparepTimer =
        {
          Unit =
            {
              Description = "Import calendar into sparep every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "hourly";
              Persistent = true;
              Unit = "${calendarSparepName}.service";
            };
        };

      schedulerConfigContents =
        schedulerCfg:
          optionalString (schedulerCfg.enable or false) ''

scheduler: ${builtins.toJSON schedulerCfg}

    '';


      schedulerSparepName = "scheduler-activate-sparep";
      schedulerSparepService =
        {
          Unit =
            {
              Description = "sparep-scheduler activation";
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "scheduler-activate-sparep-service-ExecStart"
                  ''
                    set -e
                    ${sparepPkgs.sparep-scheduler}/bin/sparep-scheduler check
                    exec ${sparepPkgs.sparep-scheduler}/bin/sparep-scheduler schedule
                  ''}";
              Type = "oneshot";
            };
        };
      schedulerSparepTimer =
        {
          Unit =
            {
              Description = "Activate sparep scheduler every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "hourly";
              Persistent = true;
              Unit = "${schedulerSparepName}.service";
            };
        };

      sparepConfigContents =
        concatStringsSep "\n" [
          (syncConfigContents cfg.sync)
          (calendarConfigContents cfg.calendar)
          (schedulerConfigContents cfg.scheduler)
          cfg.extraConfig
        ];

      services =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncSparepName}" = syncSparepService;
          }
          // optionalAttrs (cfg.backup.enable or false) {
            "${backupSparepName}" = backupSparepService;
          }
          // optionalAttrs (cfg.calendar.enable or false) {
            "${calendarSparepName}" = calendarSparepService;
          }
          // optionalAttrs (cfg.scheduler.enable or false) {
            "${schedulerSparepName}" = schedulerSparepService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncSparepName}" = syncSparepTimer;
          }
          // optionalAttrs (cfg.backup.enable or false) {
            "${backupSparepName}" = backupSparepTimer;
          }
          // optionalAttrs (cfg.calendar.enable or false) {
            "${calendarSparepName}" = calendarSparepTimer;
          }
          // optionalAttrs (cfg.scheduler.enable or false) {
            "${schedulerSparepName}" = schedulerSparepTimer;
          }
        );
      packages =
        [
          sparepPkgs.sparep
          sparepPkgs.sparep-archive
          sparepPkgs.sparep-calendar-import
          sparepPkgs.sparep-convert-org
          sparepPkgs.sparep-query
          sparepPkgs.sparep-scheduler
          sparepPkgs.sparep-single
          sparepPkgs.sparep-sync-client
        ];


    in
      mkIf cfg.enable {
        xdg = {
          configFile."sparep/config.yaml".text = sparepConfigContents;
          mimeApps = {
            defaultApplications = {
              "text/sparep" = [ "sparep.desktop" ];
              "application/sparep" = [ "sparep.desktop" ];
            };
          };
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
