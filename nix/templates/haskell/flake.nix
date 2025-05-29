{
  description = "Start of Haskell P2PRC flake";

  inputs =
    {
      nixpkgs = {
        url = "github:NixOS/nixpkgs/nixos-unstable";
      };

      flake-utils = {
        url = "github:numtide/flake-utils";
      };

      p2prc-flake = {
        url = "github:xecarlox94/p2p-rendering-computation?ref=nix";
      };

    };

  outputs = { nixpkgs, p2prc-flake, flake-utils, ... }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;

          # TODO: p2prc overlays into a single list
          overlays = [
            p2prc-flake.overlays.default
            p2prc-flake.overlays.bindings
          ];
        };


        # TODO: Add this to a p2prc-nix lib
        initProject = pkgs.writeShellApplication {
          name = "initProject";
          runtimeInputs = with pkgs; [
            ghc
            cabal2nix
            cabal-install
          ];
          text =
            let

              # TODO: finish script
#               getMainFileContent = appName: ''"\
#                 module Main where\
# \
#                 import P2PRC\
#                   ( runP2PRC\
#                   , MapPortRequest(MkMapPortRequest)\
#                   )\
# \
#                 main :: IO ()\
#                 main =\
#                   runP2PRC\
#                     ( MkMapPortRequest 8080 \"${appName}.akilan.io\"\
#                     )\
#               "'';
              # FOLDER_NAME=$(echo */ | sed 's/ /\n/' | head -n 1)
              # cat ${getMainFileContent "file_name"} > "$FOLDER_NAME"/Main.hs

            in
            ''
              # cabal init

              # sed -i 's/base.*$/base, p2prc/' haskell.cabal

              # cabal2nix . > ./cabal.nix;

              # cabal run
            '';
        };

      in {

        packages = {
          default = pkgs.haskellPackages.callPackage ./cabal.nix { };
        };

        devShells.default = pkgs.mkShell {

        # devShells.default = pkgs.haskellPackages.shellFor {

          # packages = p: [
          #   (p.callPackage ./cabal.nix { })
          # ];

          buildInputs = with pkgs; [
            p2prc-flake.packages.${system}.default
            ghc
            cabal2nix
            cabal-install
            initProject
          ];

          # TODO: add cabal2nix shell command
          shellHook = ''
            # cabal2nix . > ./cabal.nix
          '';
        };
      }
    ));

}
