#!/bin/bash

curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
cd ~/.emacs.d/
export PATH="$HOME/.cask/bin:$PATH"
cask install
git submodule update --init
