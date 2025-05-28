{
  description = "Start of Haskell P2PRC flake";

  inputs =
    {
      nixpkgs = {
        url = "github:NixOS/nixpkgs/nixos-unstable";
      };

      p2prc-flake = {
        url = "github:xecarlox94/p2p-rendering-computation?ref=nix";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      flake-utils = {
        url = "github:numtide/flake-utils";
      };

    };

  outputs = { nixpkgs, p2prc-flake, flake-utils, ... }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;

          # TODO: p2prc overlays into a single list
          overlays = [
            p2prc-flake.overlays.${system}.default
            p2prc-flake.overlays.${system}.bindings
          ];
        };


        # TODO: Add this to a p2prc-nix lib
        initProject = pkgs.writeShellApplication {
          name = "initProject";
          runtimeInputs = with pkgs; [
            cabal2nix
            cabal-install
          ];
          text = ''
            cabal init --minimal
            echo "RUNNING"

            # TODO: sed command to fix bash import and add p2prc import
            # TODO: sed command to add p2prc example to main file
            cabal2nix . > ./project.nix;

            git add .
          '';
        };

      in {
        packages = {
          # TODO: fix issue
          default = pkgs.haskellPackages.callPackage ./project.nix { };
          init = initProject;
        };

        # TODO: override haskell binding lib devshell
        # FIX: p2prc library not available in dev shell
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            cabal2nix
            cabal-install
          ];

          buildInputs = [
            p2prc-flake.packages.${system}.default
            initProject
            pkgs.haskellPackages.p2prc
          ];

          shellHook = ''
            cabal2nix . > ./project.nix
          '';
        };
      }
    ));

}
