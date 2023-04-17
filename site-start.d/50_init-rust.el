;;; 50_init-rust.el --- 50_init-rust.el -*- lexical-binding: t; coding: utf-8 -*-
;; Author: pogin503
;;; Commentary:
;; This program is free software
;;; Code:

(require 'use-package)
(require 'lsp-rust)

;; (use-package rust-mode
;;   :hook ((flyckeck-mode . flycheck-rust-setup)))

;; (use-package rustic
;;   :ensure t
;;   :config
;;   (setq rustic-lsp-server 'rls)
;;   (setq lsp-rust-analyzer-server-command '("/usr/local/bin/rust-analyzer"))
;;   (push 'rustic-clippy flycheck-checkers)
;;   (remove-hook 'rustic-mode-hook 'flycheck-mode)
;;   (setq rustic-flycheck-clippy-params "--message-format=json")
;;   )

(use-package rust-mode
  :ensure t
  :config
  (setq rust-format-on-save t)
  ;; (use-package flycheck-rust
  ;;   :config
  ;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  ;; (setq rust-rustfmt-bin )
  )

(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(provide '50_init-rust)
;;; 50_init-rust.el ends here
