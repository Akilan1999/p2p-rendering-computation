{
  description = "Nix flake for P2PRC Haskell library";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskell.packages.ghc98;
        in
        {
          packages.default = hsPkgs.callCabal2nix "p2prc" ./. {};

          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              cabal-install
              haskell.compiler.ghc98
              zlib.dev
            ];
          };
        }
    );
}
