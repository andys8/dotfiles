#!/bin/bash
set -euo pipefail

cocExtensionsPath=~/.config/coc/extensions
vimrcFile=~/dotfiles/vimrc.local

echo "Comparing CoC extensions list with installed extensions"
expected=$(grep -ioh "'coc-.*'" $vimrcFile | tr -d "'" | sort)
actual=$(jq -r ".dependencies | keys | sort | .[]" "$cocExtensionsPath/package.json")

if [[ "$expected" = "$actual" ]]; then
    echo "All good. Expected extensions and wanted extensions are matching."
else
    echo "Difference between expected and wanted extensions."
    diff <(echo "$expected") <(echo "$actual") || true
    echo "Removing extensions to trigger reinstall."
    rm -r $cocExtensionsPath
fi
