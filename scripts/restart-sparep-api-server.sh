#!/usr/bin/env bash


killall sparep-api-server || true

sparep-api-server &
