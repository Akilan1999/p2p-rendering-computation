{
  inputs = {
    naersk.url = "github:nix-community/naersk/master";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
      naersk,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        naersk-lib = pkgs.callPackage naersk { };
      in
      {

        packages = {
          default = naersk-lib.buildPackage {
            src = ./.;
            copyLibs = true;
            doCheck = true;
          };

          rust2go = pkgs.rustPlatform.buildRustPackage (finalAttrs: {
            pname = "rust2go";
            version = "0.4.3";

            src = pkgs.fetchFromGitHub {
              owner = "ihciah";
              repo = "rust2go";
              rev = "master";
              hash = "sha256-TsIMode5IRjQMEH1S8zSW+KZFslHXcQ4x/+KQCoEq0o=";
            };

            cargoHash = "sha256-o+58eYowbd+0UQPcty6YUe6kXMA98VRytdZf1kMvoDE=";

            # cargoBuildFlags = [
            # "-p"
            # "rust2go-mem-ffi"
            # ];

            buildNoDefaultFeatures = true;
            buildFeatures = [
              # "monoio"
              "tokio"
            ];

          });
        };

        # packages.default = naersk-lib.buildPackage {
        # src = ./.;
        # copyLibs = true;
        # doCheck = true;
        # };

        devShells.default =
          with pkgs;
          mkShell {
            buildInputs = [
              cargo
              rustc
              rustfmt
              pre-commit
              rustPackages.clippy
            ];
            RUST_SRC_PATH = rustPlatform.rustLibSrc;
          };

        devShell = self.devShells.${system}.default;

        defaultPackage = self.packages.${system}.default;
      }
    );
}
