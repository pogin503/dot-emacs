;;; 30_init-r-like-example --- 30_init-r-like-example
;; This program is free software
;;; Commentary:
;;; Code:

(use-package r-like-example
  :config
  (require 'elisp-examples)
  (push '("*example*" :position right :width 40 :stick t) popwin:special-display-config)
  (ex-set-keybindings))

(provide '30_init-r-like-example)
;;; 30_init-r-like-example ends here
