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
    (flake-utils.lib.eachDefaultSystem (system:
      let

        # TODO: merge overlays into a list
        # TODO: create default file in overlays folder to merge all overlays
        bindingsOverlay = import ./nix/overlays/bindings.nix;
        coreOverlay = (final: prev: {
          p2prc = final.callPackage ./. { };
        });

        # The current default sdk for macOS fails to compile go projects, so we use a newer one for now.
        # This has no effect on other platforms.
        callPackage = pkgs.darwin.apple_sdk_11_0.callPackage or pkgs.callPackage;


        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            gomod2nix.overlays.default
            bindingsOverlay
            coreOverlay
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

        overlays = {
          # TODO: merge all overlays into a single one
          default = coreOverlay;
          bindings = bindingsOverlay;
        };

      }
    )) //
    {
      templates.haskell = {
        bindings = ./nix/templates/haskell;
        description = "Haskell Bindings to p2prc protocol";
      };
    };
}
