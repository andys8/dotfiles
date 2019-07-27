#!/bin/sh
set -eu pipefail

./scripts/link.sh
./scripts/check.sh
./scripts/update.sh
./scripts/abbreviations.fish
