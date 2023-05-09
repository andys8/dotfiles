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
    0.15.8
)

current=$(psvm ls)

for v in "${versions[@]}"; do
    if [[ "$current" == *"$v"* ]]; then
        echo "$v is already installed"
    else
        echo ">> Installing purescript $v"
        psvm install "$v"
    fi
done

# psvm clean
psvm ls
psvm use $default
