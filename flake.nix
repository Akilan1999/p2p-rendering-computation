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

        # The current default sdk for macOS fails to compile go projects, so we use a newer one for now.
        # This has no effect on other platforms.
        callPackage = pkgs.darwin.apple_sdk_11_0.callPackage or pkgs.callPackage;

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            gomod2nix.overlays.default
            coreOverlay
            bindingsOverlay
          ];
        };

      in
      {

        packages.default = callPackage ./. { };

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
            nix
            git
          ];
          text =
            ''
              clear

              printf "Hello, this shell script will bootstrap a P2PRC Haskell project with Nix Flake\n"

              printf "Could you input the name of your project?\n"
              IFS= read -r project_dir

              cd \"\$project_dir\"

              git init .

              printf \"\$PWD\"

              cabal init --exe --simple

              # TODO: remove reference to cabal file
              sed -i 's/base.*$/base, p2prc/' \"\$project_dir\".cabal

              cabal2nix . > ./cabal.nix;

              # TODO: add cabal2nix shell.nix generator

              git add .

              clear

              printf "run the following commands:\n\n"

              printf "cd \"\$project_dir\"\n"
              printf "nix run github:xecarlox94/p2p-rendering-computation?ref=nix#initHaskellProject"

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
