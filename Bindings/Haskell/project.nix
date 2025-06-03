{ mkDerivation, aeson, base, bytestring, directory, lib, process
, text
}:
mkDerivation {
  pname = "p2prc";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring directory process text
  ];
  description = "P2PRC haskell library";
  license = "unknown";
}
