#!/bin/bash

cd ~/dot-emacs/
git pull
cp -r ~/dot-emacs/.emacs.d/ ~/
echo "End pull and copy"