{ mkDerivation, base, lib, servant, servant-auth
, servant-auth-client, servant-client, servant-client-core
, sparep-api, text, validity, validity-text
}:
mkDerivation {
  pname = "sparep-client";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base servant servant-auth servant-auth-client servant-client
    servant-client-core sparep-api text validity validity-text
  ];
  license = "unknown";
}
