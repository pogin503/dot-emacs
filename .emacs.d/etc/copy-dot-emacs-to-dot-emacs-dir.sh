#!/bin/bash

cd ~/dot-emacs/.emacs.d/
git pull
cp init.el*              ~/.emacs.d/
cp -r site-start.d/      ~/.emacs.d/
cp -r elisp/             ~/.emacs.d/
cp -r etc/mysnippets/    ~/.emacs.d/etc/
cp -r plugins/           ~/.emacs.d/
cp etc/*.sh              ~/.emacs.d/etc/
cd ~/.emacs.d/
rm -rf site-start.d/*.el~
rm -rf site-start.d/\#*
rm -rf site-start.d/.#*
echo "End copy dot-emacs"