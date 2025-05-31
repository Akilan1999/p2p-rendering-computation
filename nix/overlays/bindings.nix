final: prev: {
  haskellPackages = prev.haskellPackages.override {
    overrides = hfinal: hprev: {
      p2prc = hfinal.callPackage ../../Bindings/Haskell/project.nix { };
    };
  };
}
