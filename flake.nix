{
  description = "P2PRC nix flake";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils";
    };

    gomod2nix = {
      url = "github:nix-community/gomod2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      gomod2nix,
      ...
    }:
    let

      bindingsOverlay = import ./nix/overlays/bindings.nix;
      coreOverlay = (final: prev: {
        p2prc = final.callPackage ./. { };
      });

    in
    (flake-utils.lib.eachDefaultSystem (system:
      let

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            gomod2nix.overlays.default
            coreOverlay
            bindingsOverlay
          ];
        };

        # The current default sdk for macOS fails to compile go projects, so we use a newer one for now.
        # This has no effect on other platforms.
        callPackage = pkgs.darwin.apple_sdk_11_0.callPackage or pkgs.callPackage;

        p2prcDefault = callPackage ./. { };

      in
      {
        packages.default = p2prcDefault;

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            go
            gopls
            gotools
            go-tools
            gomod2nix.packages.${system}.default
            sqlite-interactive
          ];
        };

        packages.initHaskellProject = pkgs.writeShellApplication {
          name = "initHaskellProject";
          runtimeInputs = with pkgs; [
            ghc
            cabal2nix
            cabal-install
            git
            p2prcDefault
          ];
          text =
            let
              # TODO: add
              p2prcMainContent = availablePort: availableUrl:
                ''
                  module Main where

                  import P2PRC
                    ( runP2PRC
                    , MapPortRequest(MkMapPortRequest)
                    )

                  main :: IO ()
                  main =
                    runP2PRC
                      ( MkMapPortRequest ${availablePort} "${availableUrl}.akilan.io"
                      )
                '';

              mainFileContent = p2prcMainContent 8080 "haskell";
            in
            ''
              clear
              if [ "$#" -eq 0 ]; then
                echo "No arguments provided."
                echo "Please provide the name of your project"
                echo "nix run github:akilan1999/p2p-rendering-computation#initHaskellProject -- <NAME-PROJECT>"
                exit 1;
              fi

              PROJECT_DIR="$1"

              mkdir "$PROJECT_DIR"

              cd "$PROJECT_DIR"

              git init .
              clear

              cabal init --exe --simple

              sed -i 's/base.*$/base, p2prc/' "$PROJECT_DIR".cabal

              cat ${mainFileContent} > app/Main.hs

              cabal2nix . > ./cabal.nix;
              cabal2nix . --shell > shell.nix

              git add .
              clear

              echo -e "run the following commands to finish nix development and production environment:\n\n"

              echo -e "cd $PROJECT_DIR"
              echo -e "nix flake init -t github:akilan1999/p2p-rendering-computation#haskell"
              echo -e "nix develop"
              echo -e "nix run"

            '';
        };
      }
    )) //
    {
      overlays = {
        default = coreOverlay;
        bindings = bindingsOverlay;
      };
      templates.haskell = {
        path = ./nix/templates/haskell;
        description = "Haskell Bindings to p2prc protocol";
      };
    };
}
