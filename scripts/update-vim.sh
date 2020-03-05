#!/bin/bash
set -euo pipefail

# Vim plugin installation
vim +PlugUpgrade +PlugUpdate +PlugClean! +qall

echo "Vim updated"
