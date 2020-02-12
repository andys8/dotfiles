#!/bin/bash
set -euo pipefail

git fetch
git diff @ "@{upstream}"
git pull
