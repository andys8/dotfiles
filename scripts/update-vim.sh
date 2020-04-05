#!/bin/bash
set -euo pipefail

echo "Update Vim Plugins"
vim +PlugUpgrade +PlugUpdate +PlugClean! +qall

echo "Update CoC Extensions"
vim +CocInstall +qall

echo "Vim updated"
