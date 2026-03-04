#!/bin/bash
set -euo pipefail

for l in $(find ~/bin -type l); do
    if [ ! -e "$l" ]; then
        echo "Target of '$l' doesn't exist. Cleaning up."
        rm "$l"
    fi
done

echo "Cleaned up symlinks"
