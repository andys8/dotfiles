#!/bin/bash
set -euo pipefail

# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean! +qall
