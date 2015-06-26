;;; 50_mylib-haskell-mode.el --- 50_mylib-haskell-mode.el -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; This program is free software
;;; Code:

(with-eval-after-load "haskell-mode"
  '(progn
     (defun my-haskell-ac-init ()
       "Set AC-mode source."
       (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
         (auto-complete-mode t)
         (setq ac-sources '(ac-source-words-in-same-mode-buffers
                            ac-source-dictionary
                            ac-source-ghc-mod))))))

(provide '50_mylib-haskell-mode)
;;; 50_mylib-haskell-mode.el ends here
