#!/bin/bash
set -euo pipefail

git fetch
git --no-pager diff @ "@{upstream}"
git pull
