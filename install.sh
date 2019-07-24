#!/bin/sh
set -eu pipefail

./link.sh
./check.sh
./update.sh
./abbreviations.fish
