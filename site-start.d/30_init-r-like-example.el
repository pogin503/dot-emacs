(require 'r-like-example)
(require 'elisp-examples)

;; (load "~/.emacs.d/plugins/r-like-example/emacs-lisp-examples.el")

;; (define-key emacs-lisp-mode-map (kbd "s-9") 'ex-example)
;; (define-key emacs-lisp-mode-map (kbd "M-9") 'ex-example)
;; (define-key emacs-lisp-mode-map (kbd "s-0") 'ex-insert-current-buffer)
;; (define-key emacs-lisp-mode-map (kbd "C-c 0") 'ex-add-example)
;; (define-key emacs-lisp-mode-map (kbd "C-c 9") 'ex-deque-example)

(add-to-list 'popwin:special-display-config '("*example*" :position right :width 45 :dedicated t))
;; (pop popwin:special-display-config)
(global-set-key (kbd "s-9") 'ex-example)
(global-set-key (kbd "M-9") 'ex-example)
(global-set-key (kbd "s-0") 'ex-insert-current-buffer)
(global-set-key (kbd "C-c 0") 'ex-add-example)
;; (global-set-key (kbd "C-c 9") 'ex-deque-example)
(global-set-key (kbd "C-c 9") 'ex-delete-last-elem)
