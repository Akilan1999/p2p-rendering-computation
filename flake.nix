{
  description = "P2PRC nix flake";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    gomod2nix = {
      url = "github:nix-community/gomod2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    p2prc-hs = {
      url = "path:./Bindings/Haskell";
      inputs.flake-utils.follows = "flake-utils";
    };

  };

  outputs = { self, nixpkgs, flake-utils, gomod2nix, p2prc-hs, ... }:
    (flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ gomod2nix.overlays.default ];
          };

          # p2prc-hs = import (self + "/Bindings/Haskell/flake.nix") {
          #   inherit pkgs;
          #   inherit system;
          # };

          # The current default sdk for macOS fails to compile go projects, so we use a newer one for now.
          # This has no effect on other platforms.
          callPackage = pkgs.darwin.apple_sdk_11_0.callPackage or pkgs.callPackage;
        in
        {

          packages.default = callPackage ./. { };

          # packages.p2prc-hs = p2prc-hs.packages.${system}.default;

          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              go
              gopls
              gotools
              go-tools
              gomod2nix.packages.${system}.default
              sqlite-interactive
              p2prc-hs.devShells.${system}.default
            ];
          };
        })
    );
}
