{ mkDerivation, async, autodocodec, autodocodec-yaml, base, brick
, bytestring, containers, cryptohash-sha256, cursor, cursor-brick
, directory, envparse, filelock, filepath, genvalidity
, genvalidity-bytestring, genvalidity-containers, genvalidity-path
, genvalidity-sydtest, genvalidity-sydtest-persistent
, genvalidity-text, lib, microlens, monad-logger, mtl
, optparse-applicative, path, path-io, persistent
, persistent-sqlite, persistent-template, pretty-show
, random-shuffle, safe, sparep-cli, sparep-client-data, sparep-data
, sydtest, sydtest-discover, text, time, typed-process, validity
, validity-bytestring, validity-containers, validity-path
, validity-text, vty, yaml
}:
mkDerivation {
  pname = "sparep-tui";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async autodocodec autodocodec-yaml base brick bytestring containers
    cryptohash-sha256 cursor cursor-brick directory envparse filelock
    filepath microlens monad-logger mtl optparse-applicative path
    path-io persistent persistent-sqlite persistent-template
    pretty-show random-shuffle safe sparep-cli sparep-client-data
    sparep-data text time typed-process validity validity-bytestring
    validity-containers validity-path validity-text vty yaml
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base genvalidity genvalidity-bytestring genvalidity-containers
    genvalidity-path genvalidity-sydtest genvalidity-sydtest-persistent
    genvalidity-text sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "sparep-tui";
}
