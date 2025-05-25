{
  description = "P2PRC nix flake";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
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

            # The current default sdk for macOS fails to compile go projects, so we use a newer one for now.
            # This has no effect on other platforms.
            callPackage = pkgs.darwin.apple_sdk_11_0.callPackage or pkgs.callPackage;

            p2prcFinalPackage = callPackage ./. { };

            pkgs = import nixpkgs {
              inherit system;
              overlays = [
                gomod2nix.overlays.default
                mainOverlay
                (final: prev: {
                  p2prc = p2prcFinalPackage;
                })
              ];
            };

          in
          {

            packages.default = p2prcFinalPackage;


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

          }
      ));
}
