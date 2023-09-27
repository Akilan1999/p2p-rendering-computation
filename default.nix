{ nixpkgs ? import <nixpkgs> {  } }:

let
  pkgs = [
    nixpkgs.go
    nixpkgs.tmux
  ];

in
  nixpkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs = pkgs;
  }
