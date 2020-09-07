#!/bin/sh
set -eu

command -v "refactor" >/dev/null 2>&1 && {
    VERSION=$(refactor --version)
    echo "apply-refact (refactor for hlint) is already installed."
    echo "$VERSION"
    exit 0
}

# install with stack
echo ">> Installing apply-refact with stack"
cd /tmp
stack install apply-refact --resolver nightly-2020-08-03
refactor --version
echo ""
echo ">> apply-refact (refactor) installed"
exit 0
