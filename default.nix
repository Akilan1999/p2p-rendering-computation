{ nixpkgs ? import <nixpkgs> {  } }:

let
  pkgs = [
    nixpkgs.go
    nixpkgs.tmux
    nixpkgs.docker
    nixpkgs.vim
  ];

in
  nixpkgs.stdenv.mkDerivation {
    name = "env";
    buildInputs = pkgs;
    pure-eval = true;
    shellHook =
    ''
        make
        export P2PRC=$PWD
        export PATH=$PWD:$PATH
    '';
  }
