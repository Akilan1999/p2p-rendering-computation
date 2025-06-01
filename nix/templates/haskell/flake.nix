{
  description = "Start of Haskell P2PRC flake";

  inputs =
    {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

      flake-util.url = "github:numtide/flake-utils";

      p2prc-flake.url = "github:xecarlox94/p2p-rendering-computation?ref=nix";
    };

  outputs = { nixpkgs, p2prc-flake, flake-utils, ... }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            p2prc-flake.overlays.default
            p2prc-flake.overlays.bindings
          ];
        };

      in {

        packages.default = pkgs.haskellPackages.callPackage ./cabal.nix { };

        devShells.default = pkgs.haskellPackages.shellFor {

          packages = p: [
            (p.callPackage ./cabal.nix { })
          ];

          buildInputs = with pkgs; [
            p2prc-flake.packages.${system}.default
            ghc
            cabal2nix
            cabal-install
          ];

          # TODO: add cabal2nix shell command
          # cabal2nix . --shell > ./shell.nix
          shellHook = ''
            cabal2nix . > ./cabal.nix
          '';
        };
      }
    ));
}
