{ mkDerivation, aeson, appendful, appendful-persistent, autodocodec
, base, lib, password, password-instances, persistent
, persistent-sqlite, persistent-template, sparep-data, text, time
, validity, validity-persistent, validity-text
}:
mkDerivation {
  pname = "sparep-api-server-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson appendful appendful-persistent autodocodec base password
    password-instances persistent persistent-sqlite persistent-template
    sparep-data text time validity validity-persistent validity-text
  ];
  license = "unknown";
}
