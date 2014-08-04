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

;; (pop popwin:special-display-config)
(global-set-key (kbd "s-9") 'ex-example)
(global-set-key (kbd "M-9") 'ex-example)
(global-set-key (kbd "s-0") 'ex-store-key-example)
(global-set-key (kbd "C-c 0 a") 'ex-add-example)
(global-set-key (kbd "C-c 0 i") 'ex-insert-current-buffer)
(global-set-key (kbd "C-c 0 p") 'ex-put-to-example)
;; (global-set-key (kbd "C-c 9") 'ex-deque-example)
(global-set-key (kbd "C-c 0 d") 'ex-delete-last-elem)

(provide '30_init-r-like-example)
;;; 30_init-r-like-example ends here
