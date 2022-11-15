{ mkDerivation, base, bytestring, cookie, genvalidity-sydtest
, http-client, http-types, lib, monad-logger, persistent
, persistent-sqlite, QuickCheck, servant, servant-auth-client
, servant-auth-server, servant-client, sparep-api, sparep-api-gen
, sparep-api-server, sparep-api-server-data
, sparep-api-server-data-gen, sparep-client, sparep-data
, sparep-data-gen, sydtest, sydtest-discover, text, warp
}:
mkDerivation {
  pname = "sparep-api-server-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring cookie genvalidity-sydtest http-client http-types
    monad-logger persistent persistent-sqlite QuickCheck servant
    servant-auth-client servant-auth-server servant-client sparep-api
    sparep-api-gen sparep-api-server sparep-api-server-data
    sparep-api-server-data-gen sparep-client sparep-data
    sparep-data-gen sydtest text warp
  ];
  testHaskellDepends = [
    base bytestring cookie genvalidity-sydtest http-client http-types
    monad-logger persistent persistent-sqlite QuickCheck servant
    servant-auth-client servant-auth-server servant-client sparep-api
    sparep-api-gen sparep-api-server sparep-api-server-data
    sparep-api-server-data-gen sparep-client sparep-data
    sparep-data-gen sydtest text warp
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
