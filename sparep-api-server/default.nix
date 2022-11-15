{ mkDerivation, aeson, appendful, appendful-persistent, autodocodec
, autodocodec-yaml, base, bytestring, envparse, jose, lib
, monad-logger, mtl, optparse-applicative, password
, password-instances, path, path-io, persistent, persistent-sqlite
, persistent-template, servant-auth-server, servant-server
, sparep-api, sparep-api-server-data, sparep-data, text, time, wai
, warp, yaml
}:
mkDerivation {
  pname = "sparep-api-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson appendful appendful-persistent autodocodec autodocodec-yaml
    base bytestring envparse jose monad-logger mtl optparse-applicative
    password password-instances path path-io persistent
    persistent-sqlite persistent-template servant-auth-server
    servant-server sparep-api sparep-api-server-data sparep-data text
    time wai warp yaml
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "sparep-api-server";
}
