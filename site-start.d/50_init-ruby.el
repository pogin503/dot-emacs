;; 50_init-ruby.el --- ruby conf
;;; Commentary:
;;; Code:
(require 'use-package)
(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
         ("Capfile$" . ruby-mode)
         ("Gemfile$" . ruby-mode))
  :config
  ;; ruby-block
  (setq ruby-indent-level 2)
  (setq ruby-indent-tabs-mode nil)
  (custom-set-variables
   '(inf-ruby-default-implementation "pry")
   '(inf-ruby-eval-binding "Pry.toplevel_binding")
   '(ruby-insert-encoding-magic-comment nil)))

(use-package ruby-end
  :config
  (add-to-list 'auto-mode-alist '(".erb$" . rhtml-mode)))

(use-package inf-ruby
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package haml-move
  :defer t)

(provide '50_init-ruby)
;;; 50_init-ruby.el ends here
