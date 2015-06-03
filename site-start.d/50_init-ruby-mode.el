;; 50_init-ruby-mode.el --- ruby conf
;;; Commentary:
;;; Code:

(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(eval-after-load 'ruby-mode
  (progn
    ;; ruby-block
    ;; ミニバッファに表示し, かつ, オーバレイする.
    (setq ruby-block-highlight-toggle t)
    (setq ruby-indent-level 2)
    (setq ruby-indent-tabs-mode nil)
    ))

(eval-after-load 'ruby-end
  (add-to-list 'auto-mode-alist '(".erb$" . rhtml-mode)))

(eval-after-load 'haml-mode
  (require 'haml-mode))

;;; 50_init-ruby-mode.el ends here
