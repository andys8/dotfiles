#!/bin/bash
# Update brew
set -eu

echo "Update asdf"

if ! command -v asdf &>/dev/null; then
    echo "asdf not found"
    exit 0
fi

asdf plugin update --all
asdf reshim
