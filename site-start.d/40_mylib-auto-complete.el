;;; 40_mylib-auto-complete.el --- 40_mylib-auto-complete.el
;;; Commentary:
;; This program is free software
;;; Code:

(defun my-ac-eshell-mode ()
  (setq ac-sources
        '(ac-source-pcomplete
          ac-source-words-in-buffer
          ac-source-dictionary)))

(eval-after-load "haskell-mode"
  '(progn
     (defun my-ac-haskell-mode ()
       (setq ac-sources '(ac-source-words-in-same-mode-buffers
                          ac-source-dictionary
                          ac-source-ghc-mod)))))
;; coq
(defun my-ac-coq-mode ()
  "Set coq ac-source."
  (setq ac-sources '(ac-source-words-in-same-mode-buffers
                     ac-source-dictionary)))

(provide '40_mylib-auto-complete)
;;; 40_mylib-auto-complete.el ends here
