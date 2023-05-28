;;; 50_init-python.el --- 50_init-python.el -*- lexical-binding: t; coding: utf-8 -*-
;; Author: iMac
;; Version:
;; Package-Requires: ()
;;; Commentary:
;; This program is free software
;;; Code:

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(provide '50_init-python)
;;; 50_init-python.el ends here
