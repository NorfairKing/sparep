{ mkDerivation, base, bytestring, genvalidity
, genvalidity-persistent, genvalidity-sydtest, genvalidity-text
, lib, password, QuickCheck, sparep-api-server-data
, sparep-data-gen, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "sparep-api-server-data-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring genvalidity genvalidity-persistent genvalidity-text
    password QuickCheck sparep-api-server-data sparep-data-gen text
  ];
  testHaskellDepends = [
    base genvalidity-sydtest sparep-api-server-data sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
