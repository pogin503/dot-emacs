;; 50_init-ruby.el --- ruby conf
;;; Commentary:
;;; Code:

(use-package ruby-mode
  :mode (("\\.rb$latex " . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode))
  :config
  ;; ruby-block
  ;; ミニバッファに表示し, かつ, オーバレイする.
  (setq ruby-block-highlight-toggle t)
  (setq ruby-indent-level 2)
  (setq ruby-indent-tabs-mode nil))

(use-package ruby-end
  :config
  (add-to-list 'auto-mode-alist '(".erb$" . rhtml-mode)))

(use-package haml-move
  :defer t)

(provide '50_init-ruby)
;;; 50_init-ruby.el ends here
