#!/bin/bash
set -euo pipefail
# Clean symlinks where target file doesn't exist

symlinks=$(find ~/bin -type l)

for l in $symlinks; do
    if ! readlink -e "$l" &>/dev/null; then
        echo "Target of '$l' doesn't exist. Cleaning up."
        rm "$l"
    fi
done

echo "Cleaned up symlinks"
