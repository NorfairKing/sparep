{ mkDerivation, base, genvalidity, genvalidity-bytestring
, genvalidity-containers, genvalidity-path, genvalidity-sydtest
, genvalidity-sydtest-persistent, genvalidity-text
, genvalidity-time, lib, path, path-io, sparep-data, sydtest
, sydtest-discover, text
}:
mkDerivation {
  pname = "sparep-data-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-bytestring genvalidity-containers
    genvalidity-path genvalidity-text genvalidity-time sparep-data text
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-persistent path
    path-io sparep-data sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
