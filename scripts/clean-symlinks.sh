#!/bin/bash
set -euo pipefail
# Clean symlinks where target file doesn't exist
#
# TODO: Script is not working under macos
echo "Skip clean symlinks under macos (TODO)"
exit 0

symlinks=$(find ~/bin -type l)

for l in $symlinks; do
    if ! readlink -e "$l" &>/dev/null; then
        echo "Target of '$l' doesn't exist. Cleaning up."
        rm "$l"
    fi
done

echo "Cleaned up symlinks"
