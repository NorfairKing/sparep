{ mkDerivation, autodocodec, autodocodec-yaml, base, data-default
, envparse, lib, monad-logger, optparse-applicative, path, path-io
, shakespeare, sparep-api, sparep-client, sydtest, sydtest-discover
, sydtest-yesod, template-haskell, text, yaml, yesod
}:
mkDerivation {
  pname = "sparep-web-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base data-default envparse
    monad-logger optparse-applicative path path-io shakespeare
    sparep-api sparep-client template-haskell text yaml yesod
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base monad-logger sydtest sydtest-yesod ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "sparep-web-server";
}
