#!/bin/bash

cd ~/.emacs.d/
cp -r ~/.emacs.d/site-start.d/   ~/dot-emacs/.emacs.d/
cp -r ~/.emacs.d/elisp/          ~/dot-emacs/.emacs.d/
cp -r ~/.emacs.d/etc/mysnippets/ ~/dot-emacs/.emacs.d/etc/
cp -r ~/.emacs.d/plugins/        ~/dot-emacs/.emacs.d/
cd ~/dot-emacs/
rm -rf .emacs.d/site-start.d/*.el~
cd ~/dot-emacs/.emacs.d/
rm -rf site-start.d/\#*
rm -rf site-start.d/.#*
echo "End copy .emacs.d"
