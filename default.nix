{ pkgs ? (
    let
      inherit (builtins) fetchTree fromJSON readFile;
      inherit ((fromJSON (readFile ./flake.lock)).nodes) nixpkgs gomod2nix;
    in
    import (fetchTree nixpkgs.locked) {
      overlays = [
        (import "${fetchTree gomod2nix.locked}/overlay.nix")
      ];
    }
  ),
  lib
}:

pkgs.buildGoApplication {
  pname = "P2PRC";
  version = "2.0.0";
  pwd = ./.;
  src = ./.;
  modules = ./gomod2nix.toml;
  doCheck = false;

  buildInputs = [ pkgs.makeWrapper ];

  postBuild = ''
    wrapProgram $out/bin/p2p-rendering-computation \
      --set PATH  $out/bin \
      --set P2PRC $out/bin
  '';

}
