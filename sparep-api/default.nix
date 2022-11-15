{ mkDerivation, aeson, appendful, autodocodec, base, jose, lib
, persistent, servant, servant-auth, servant-auth-server
, sparep-api-server-data, sparep-client-data, sparep-data, text
, validity, validity-text
}:
mkDerivation {
  pname = "sparep-api";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson appendful autodocodec base jose persistent servant
    servant-auth servant-auth-server sparep-api-server-data
    sparep-client-data sparep-data text validity validity-text
  ];
  license = "unknown";
}
