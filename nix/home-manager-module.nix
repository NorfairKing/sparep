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
          sparepReleasePackages = mkOption {
            description = "The sparepReleasePackages attribute defined in the nix/overlay.nix file in the sparep repository.";
            default = (import ./pkgs.nix { }).sparepReleasePackages;
          };
          decks =
            mkOption {
              type = types.listOf types.str;
              example = [ "~/decks" ];
              default = [ ];
              description = "Where to find the decks to study";
            };
          completion-command =
            mkOption {
              type = types.nullOr types.str;
              example = "habitscipline-cli entry sparep";
              default = null;
              description = "The command to run upon completing a study session";
            };
          extraConfig =
            mkOption {
              default = { };
              description = "Extra contents for the config file";
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
      configContents = with cfg;
        optionalAttrs (decks != [ ]) { inherit decks; } // optionalAttrs (!builtins.isNull cfg.completion-command) { inherit completion-command; } // syncConfigContents sync // extraConfig;
      syncConfigContents = syncCfg: with syncCfg;
        optionalAttrs (syncCfg.enable) {
          inherit server-url;
          inherit username;
          inherit password;
        };
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
              ExecStart = "${pkgs.writeShellScript "sync-sparep" ''
                exec ${cfg.sparepReleasePackages.sparep-cli}/bin/sparep sync
              ''}";
              Type = "oneshot";
            };
        };
      syncSparepTimer =
        {
          Unit =
            {
              Description = "Sync sparep every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "daily";
              Persistent = true;
              Unit = "${syncSparepName}.service";
            };
        };

      sparepConfigContents = builtins.toJSON configContents;
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
      packages = with cfg.sparepReleasePackages;
        [
          sparep-cli
          sparep-tui
        ];


    in
    mkIf cfg.enable {
      xdg = {
        configFile."sparep/config.yaml".text = sparepConfigContents;
      };
      systemd.user =
        {
          services = services;
          timers = timers;
        };
      home.packages = packages;
    };
}
