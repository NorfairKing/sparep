{ mkDerivation, base, genvalidity-sydtest
, genvalidity-sydtest-persistent, lib, persistent
, persistent-sqlite, persistent-template, safe
, sparep-api-server-data, sparep-data, sydtest, sydtest-discover
, time
}:
mkDerivation {
  pname = "sparep-client-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base persistent persistent-sqlite persistent-template safe
    sparep-api-server-data sparep-data time
  ];
  testHaskellDepends = [
    base genvalidity-sydtest genvalidity-sydtest-persistent sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
