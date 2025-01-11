{
  description = "Nix flake for P2PRC Haskell library";

  inputs = {
    p2prc.url = "../../";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, p2prc }: utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          cabal-install
          haskell.compiler.ghc96
          zlib.dev
          p2prc.outputs.packages.${system}.default
        ];
      };
    }
  );
}
