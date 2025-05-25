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
        in
        {
          packages.default = pkgs.haskellPackages.callPackage ./project.nix {};

          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              cabal2nix
              cabal-install
              haskell.compiler.ghc98
              zlib.dev
            ];

            shellHook = ''
              pwd
              cd ../../
              echo "building package"
              pwd
              nix build .
              echo "building package"
              pwd
              cd Bindings/Haskell
              pwd
              echo "done!!!!"
            '';
          };
        }
    );
}
