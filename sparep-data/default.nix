{ mkDerivation, aeson, autodocodec, autodocodec-yaml, base, base16
, bytestring, containers, cryptohash-sha256, directory, filepath
, lib, path, path-io, persistent, random-shuffle, safe, text, time
, validity, validity-bytestring, validity-containers, validity-path
, validity-text, validity-time, yaml
}:
mkDerivation {
  pname = "sparep-data";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-yaml base base16 bytestring
    containers cryptohash-sha256 directory filepath path path-io
    persistent random-shuffle safe text time validity
    validity-bytestring validity-containers validity-path validity-text
    validity-time yaml
  ];
  license = "unknown";
}
