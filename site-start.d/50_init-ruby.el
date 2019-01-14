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

(defun my-ruby-mode-conf ()
  (setq company-auto-expand t)
  (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
  (setq company-idle-delay 0) ; 遅延なしにすぐ表示
  (setq company-minimum-prefix-length 1) ; 何文字打つと補完動作を行うか設定
  (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
  (setq completion-ignore-case t)
  (setq company-dabbrev-downcase nil)
  (my-ruby-mode-keybinds)
  )

(use-package company
  :config
  (add-hook 'ruby-mode-hook #'my-ruby-mode-conf)
  )
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
