#!/usr/bin/env bash

export PATH="$PATH:$(stack path --local-install-root)/bin"
export DEVELOPMENT=True
cd sparep-web-server
stack build \
  --file-watch \
  --exec='../scripts/restart-sparep-web-server.sh' \
  --no-nix-pure \
  $*
