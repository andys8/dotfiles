#!/bin/bash
# Update the OS
set -eu

echo "Update brew"

brew update 
brew upgrade
