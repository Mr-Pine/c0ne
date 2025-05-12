#!/usr/bin/env sh
set -e
cd "$(dirname "$0")"
gradle --no-daemon installDist
