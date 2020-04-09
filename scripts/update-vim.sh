#!/bin/bash
set -euo pipefail

echo "$(tput setaf 8)Update Vim plugins$(tput sgr 0)"
vim +PlugUpgrade +PlugUpdate +PlugClean! +qall

echo "$(tput setaf 8)Update CoC extensions$(tput sgr 0)"
vim +CocInstall +qall

echo "Vim updated"
