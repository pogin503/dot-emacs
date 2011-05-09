#!/bin/bash

cd ~/.emacs.d/
cp -r ~/.emacs.d/site-start.d/ ~/dot-emacs/.emacs.d/
cp -r ~/.emacs.d/elisp/ ~/dot-emacs/.emacs.d/
cp -r ~/.emacs.d/etc/mysnippets/ ~/dot-emacs/.emacs.d/etc/
cp -r ~/.emacs.d/plugins/ ~/dot-emacs/.emacs.d/
echo "End copy .emacs.d"