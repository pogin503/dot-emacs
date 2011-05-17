#!/bin/bash

cd ~/.emacs.d/
cp -r site-start.d/   ~/dot-emacs/.emacs.d/
cp -r elisp/          ~/dot-emacs/.emacs.d/
cp -r etc/mysnippets/ ~/dot-emacs/.emacs.d/etc/
cp -r plugins/        ~/dot-emacs/.emacs.d/
cp etc/*.sh
cd ~/dot-emacs/.emacs.d/
rm -rf site-start.d/*.el~
rm -rf site-start.d/\#*
rm -rf site-start.d/.#*
echo "End copy .emacs.d"
