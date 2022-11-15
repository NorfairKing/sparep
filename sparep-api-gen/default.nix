{ mkDerivation, base, genvalidity, genvalidity-appendful
, genvalidity-sydtest, genvalidity-text, lib, sparep-api
, sparep-api-server-data-gen, sydtest, sydtest-discover, text
}:
mkDerivation {
  pname = "sparep-api-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-appendful genvalidity-text sparep-api
    sparep-api-server-data-gen text
  ];
  testHaskellDepends = [
    base genvalidity-sydtest sparep-api sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
