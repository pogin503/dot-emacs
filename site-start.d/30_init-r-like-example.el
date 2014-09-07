;;; 30_init-r-like-example --- 30_init-r-like-example
;; This program is free software
;;; Commentary:
;;; Code:
(require 'r-like-example)
(require 'elisp-examples)

;; (load "~/.emacs.d/plugins/r-like-example/emacs-lisp-examples.el")

;; (define-key emacs-lisp-mode-map (kbd "s-9") 'ex-example)
;; (define-key emacs-lisp-mode-map (kbd "M-9") 'ex-example)
;; (define-key emacs-lisp-mode-map (kbd "s-0") 'ex-insert-current-buffer)
;; (define-key emacs-lisp-mode-map (kbd "C-c 0") 'ex-add-example)
;; (define-key emacs-lisp-mode-map (kbd "C-c 9") 'ex-deque-example)

;; (add-to-list 'popwin:special-display-config '("*example*" :position right :width 45 :stick t))
(push '("*example*" :position right :width 45 :stick t) popwin:special-display-config)

(ex-set-keybinding)

(provide '30_init-r-like-example)
;;; 30_init-r-like-example ends here
