{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.url = "github:therishidesai/nixpkgs/rdesai/haskell-fanout";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;

        packages.pubmsg = flake.packages."baf:exe:pubmsg";
        packages.submsg = flake.packages."baf:exe:submsg";

        packages.baf-test = pkgs.nixosTest {
          name = "ipc-regression-test";
          nodes.machine = { config, lib, ... }: {
            imports = lib.attrValues self.nixosModules;

            config = {
              users.users = {
                baf = {
                  isNormalUser = true;
                  password = "baf";
                  description = "baf";
                  extraGroups = [ "networkmanager" "wheel" "dialout" "i2c" ];
                };
              };

              baf.services.baf-setup = {
                enable = true;
                topics = [ "test" ];
              };
              
              environment.systemPackages = [
                pkgs.tmux
                self.packages."${system}".pubmsg
                self.packages."${system}".submsg
              ];
            };

          };

          testScript = ''
            start_all()
            machine.wait_for_unit("baf.service")
          '';
        };
      }) // {
        nixosModules.default = { config, lib, pkgs, ... }:
          with lib;
          let cfg = config.baf.services.baf-setup;
              pubs = [ "baf-test" ] ++ cfg.topics;
              lnPubs = lists.imap0 (i: p: "ln -s /dev/fanout${builtins.toString i} /dev/baf/${p}") pubs;
          in {
            options.baf.services.baf-setup = {
              enable = mkEnableOption "Enable the big ass fan!";
              topics = mkOption {
                type = types.listOf types.str;
                default = [ ];
                description = "A list of publisher topics";
              };
              bufferSize = lib.mkOption {
                type = lib.types.int;
                default = 16384;
                description = "Size of /dev/fanout buffer in bytes";
              };
            };

            config = mkIf cfg.enable {
              services.fanout = {
                enable = true;
                fanoutDevices = builtins.length pubs;
                bufferSize = cfg.bufferSize;
              };

              systemd.tmpfiles.rules = [
                "d /dev/baf 0755 root root"
              ];

              systemd.services.baf-setup = {
                description = "Turn on the big ass fan!";
                script = strings.concatLines lnPubs;

                after = [ "fanout.service" ];
                requires = [ "fanout.service" ];
                wantedBy = [ "multi-user.target" ];
                serviceConfig = {
                  Type = "oneshot";
                  User = "root";
                  RemainAfterExit = "yes";
                  Restart = "no";
                };
              };
            };
          };
      };

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
