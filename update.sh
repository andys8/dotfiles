#!/usr/bin/env bash

# Vim plugin installation
vim +PlugUpgrade +PlugInstall +PlugUpdate +PlugClean +qall

# Setup YouCompleteMe
python3 ~/.vim/plugged/youcompleteme/install.py --ts-completer --java-completer

