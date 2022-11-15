{ mkDerivation, appendful, appendful-persistent, autodocodec
, autodocodec-yaml, base, cookie, envparse, filelock, genvalidity
, genvalidity-sydtest, http-client, http-client-tls, lib
, monad-logger, mtl, optparse-applicative, path, path-io
, persistent, persistent-sqlite, servant, servant-auth-client
, servant-client, sparep-api, sparep-api-server-data
, sparep-api-server-gen, sparep-client, sparep-client-data
, sparep-data, sydtest, sydtest-discover, text, yaml
}:
mkDerivation {
  pname = "sparep-cli";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    appendful appendful-persistent autodocodec autodocodec-yaml base
    cookie envparse filelock http-client http-client-tls monad-logger
    mtl optparse-applicative path path-io persistent persistent-sqlite
    servant servant-auth-client servant-client sparep-api
    sparep-api-server-data sparep-client sparep-client-data sparep-data
    text yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base genvalidity genvalidity-sydtest path path-io servant-client
    sparep-api sparep-api-server-data sparep-api-server-gen sydtest
    text
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "sparep";
}
