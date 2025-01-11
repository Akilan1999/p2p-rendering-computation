#!/bin/bash

rm -rf dist-newstyle/

cabal haddock

rm -rf ../../Docs/haskell

cp -r \
  ./dist-newstyle/build/x86_64-linux/ghc-9.6.6/p2prc-0.1.0.0/doc/html/p2prc/ \
  ../../Docs/haskell
