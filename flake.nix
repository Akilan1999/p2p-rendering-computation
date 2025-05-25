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
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      gomod2nix,
      p2prc-hs,
      ...
    }:
    let

      getOverlay = overlayName: import ./nix/overlays/${overlayName};

      mainOverlay = getOverlay "main.nix";

    in
    {
      overlays.default = mainOverlay;
    }
    //
    (flake-utils.lib.eachDefaultSystem (
        system:
          let
            pkgs = import nixpkgs {
              inherit system;
              overlays = [ gomod2nix.overlays.default mainOverlay ];
            };

            # The current default sdk for macOS fails to compile go projects, so we use a newer one for now.
            # This has no effect on other platforms.
            callPackage = pkgs.darwin.apple_sdk_11_0.callPackage or pkgs.callPackage;
          in
          {

            packages.default = callPackage ./. { };

            packages.p2prc-hs = pkgs.haskellPackages.callPackage ./Bindings/Haskell/project.nix { };

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

            devShells.p2prc-hs = p2prc-hs.devShell.${system};

          }
      ));
}
