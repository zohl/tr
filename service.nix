{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.tr;

  tr-package = import ./shell.nix {};

  tr-config = pkgs.writeText "tr.ini" ''
    [Dictionaries]
    ${optionalString (cfg.dictionaries.path != null) "path=${cfg.dictionaries.path}"}
  '';
 in {

  ###### interface

  options = {

    services.tr = {

      enable = mkEnableOption "tr";

      user = mkOption {
        type = types.str;
        default = "nginx";
        example = "nginx";
        description = ''
          User account under which the applicaton runs.
        '';
      };

      dictionaries = {
        path = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Where to get dictionaries.
          '';
        };
      };
    };
  };


  ###### implementation

  config = mkIf cfg.enable {

    systemd.services.tr = {

        description = "Tr service";

        preStart = "";

        serviceConfig = {
          User = "${cfg.user}";
          ExecStart = "${tr-package}/bin/tr -c ${tr-config}";
          StandardOutput = "syslog";
          StandardError = "syslog";
          PermissionsStartOnly = true;
        };

        wantedBy = ["multi-user.target"];
        after = ["network.target"];
    };
  };
}
