#!/bin/bash
set -euo pipefail

cd ~/dotfiles
git fetch
git --no-pager diff @ "@{upstream}"
git pull
