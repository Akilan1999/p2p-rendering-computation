{
  description = "Nix flake for P2PRC Haskell library";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    p2prc-main = "path:../../";
  };

  outputs =
    { nixpkgs, flake-utils, p2prc-main-go, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          packages.default = pkgs.haskellPackages.callPackage ./project.nix {};

          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              cabal2nix
              cabal-install
              haskell.compiler.ghc98
              zlib.dev
              p2prc-main-go.packages.${system}.default
            ];

          };
        }
    );
}
