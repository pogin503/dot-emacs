;;; 30_init-r-like-example --- 30_init-r-like-example
;; This program is free software
;;; Commentary:
;;; Code:

(require 'r-like-example)
(require 'elisp-examples)

;; (add-to-list 'popwin:special-display-config '("*example*" :position right :width 45 :stick t))
(push '("*example*" :position right :width 45 :stick t) popwin:special-display-config)

(ex-set-keybindings)

(provide '30_init-r-like-example)
;;; 30_init-r-like-example ends here
