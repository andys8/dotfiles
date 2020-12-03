#!/bin/bash
set -euo pipefail

stack upgrade
stack update

if [ $((RANDOM % 10)) -eq 0 ]; then
    echo "Update hoogle"
    hoogle generate
fi
