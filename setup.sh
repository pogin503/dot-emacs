#!/bin/bash

curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python
cd ~/.emacs.d
export PATH="/home/vagrant/.cask/bin:$PATH"
cask install
