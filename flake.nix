{
  description = "Description for the project";

  # Binary server configuration
  nixConfig = {
    extra-experimental-features = "nix-command flakes";
    extra-substituters = "https://nix-cache.cajun.page/public https://pre-commit-hooks.cachix.org";
    extra-trusted-public-keys = "public:Ts+1e+F/BjkLKF/7eqbHa7x/wKWXA5PzU8bVRBy0ysU= pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc=";
    # netrc-file = ./netrc; # Use if cache is private
  };

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs @ {
    flake-parts,
    nixpkgs,
    pre-commit-hooks-nix,
    ...
  }: let
    pname = "socket-effectful"; # Your cabal project's name
    buildProject = false; # Include your project (useful for cabal init)
  in
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-darwin"];
      imports = [
        # Ensure the extra-substituters is correctly configured, otherwise the
        # entire world will be rebuilt :3
        pre-commit-hooks-nix.flakeModule
      ];
      perSystem = {
        config,
        pkgs,
        system,
        self',
        ...
      }: let
        hp =
          if buildProject
          then
            pkgs.haskellPackages.override {
              overrides = final: prev: {
                ${pname} = final.callCabal2nix pname ./. {};
              };
            }
          else pkgs.haskellPackages;
      in {
        ########################################################################
        ##                       PRIMARY CONFIGURATION                        ##
        ########################################################################
        formatter = pkgs.alejandra;

        packages.default =
          if buildProject
          then hp.${pname}
          else pkgs.hello;

        devShells.default = hp.shellFor {
          packages = hpkgs:
            with hpkgs; (
              if buildProject
              then [self'.packages.default]
              else []
            );
          nativeBuildInputs = with hp;
          with pkgs; [
            cabal-install
            haskell-language-server
            pre-commit
          ];
          shellHook = ''
            ${config.pre-commit.installationScript}
            echo 1>&2 "Welcome to the development shell!"
          '';
        };

        ########################################################################
        ##                          PRE-COMMIT HOOKS                          ##
        ########################################################################
        pre-commit = {
          check.enable = true;
          settings = {
            hooks = {
              fourmolu.enable = true;
              cabal-gild.enable = true;

              alejandra.enable = true;
              check-symlinks.enable = true;
              trim-trailing-whitespace.enable = true;
            };
          };
        };

        ########################################################################
        _module.args.pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
        };
      };
    };
}
