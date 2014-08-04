;; 50_init-ruby-mode.el --- ruby conf
;;; Commentary:
;;; Code:
;(auto-install-from-url "http://tromey.com/elpa/package-install.el")

(require '00_init-macro)

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;; (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
;; (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(eval-after-load 'ruby-mode
  (progn
    ;; ruby-block
    ;; (req ruby-block)
    ;; (ruby-block-mode t)
    ;; ミニバッファに表示し, かつ, オーバレイする.
    (setq ruby-block-highlight-toggle t)
    ;; (defun ruby-mode-hook-ruby-block()
    ;;   (ruby-block-mode t))
    (req ruby-electric)
    ;; set ruby-mode indent
    (setq ruby-indent-level 2)
    (setq ruby-indent-tabs-mode nil)
    ))

;; ruby-electric
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode 1)))

;; (setq ruby-electric-expand-delimiters-list nil)

;; (add-hook 'ruby-mode-hook 'ruby-mode-hook-ruby-block)

(require 'mylib)
(require 'ruby-end)
(add-to-list 'auto-mode-alist '(".erb$" . rhtml-mode))

;; (defun html-setting ()
;;   (setq c-basic-offset 1
;;         tab-width 1))

;; (add-hook 'rhtml-mode-hook 'html-setting)
;; (require 'rhtml-mode)
;; (require 'rinari)
;; (add-hook 'rhtml-mode-hook
;;     (lambda () (rinari-launch)))

(req haml-mode)

;;; 50_init-ruby-mode.el ends here
