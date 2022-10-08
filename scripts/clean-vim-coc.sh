#!/bin/bash
set -euo pipefail

cocExtensionsPath=~/.config/coc/extensions
vimrcFile=~/dotfiles/vimrc.local

if [ ! -f "$cocExtensionsPath/package.json" ]; then
    echo "Skipping because coc package.json missing. Start vim and wait for installation."
    exit 0
fi

echo "Comparing CoC extensions list with installed extensions"
expected=$(grep -ioh "'\(@.*/\)\?coc-.*'" $vimrcFile | tr -d "'" | sort)
actual=$(jq -r ".dependencies | keys | .[]" "$cocExtensionsPath/package.json" | sort)

if [[ "$expected" = "$actual" ]]; then
    echo "All good. Expected extensions and wanted extensions are matching."
else
    echo "Difference between expected and wanted extensions."
    diff <(echo "$expected") <(echo "$actual") || true
    echo "Removing extensions to trigger reinstall."
    rm -r $cocExtensionsPath
fi
