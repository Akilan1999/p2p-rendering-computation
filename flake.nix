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
      coreOverlay = (
        final: prev: {
          p2prc = final.callPackage ./. { };
        }
      );

    in
    (flake-utils.lib.eachDefaultSystem (
      system:
      let

        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            gomod2nix.overlays.default
            coreOverlay
            bindingsOverlay
          ];
        };

        p2prcDefault = pkgs.callPackage ./. { };

      in
      {

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

        packages.default = p2prcDefault;

        packages.initHaskellProject = import ./nix/scripts { inherit pkgs p2prcDefault; };

      }
    ))
    // {
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
