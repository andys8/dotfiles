#!/bin/bash
# Install PureScript compiler versions
set -euo pipefail

# Default compiler version
default=0.15.4

# PureScript compiler versions
versions=(
    0.15.4
    0.15.6
    0.15.7
)

for v in "${versions[@]}"; do
    echo ">> Installing purescript $v"
    psvm install "$v"
done

# psvm clean
psvm ls
psvm use $default
