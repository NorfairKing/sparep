#!/usr/bin/env bash

stack install sparep-api-server \
  --file-watch \
  --exec='./scripts/restart.sh'
