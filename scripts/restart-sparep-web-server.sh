#!/usr/bin/env bash

killall sparep-web-server || true


sparep-web-server serve &
