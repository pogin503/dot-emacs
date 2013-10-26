;;; 50_init-ruby-mode.el --- ruby conf
;;; Commentary:
;;; Code:
;(auto-install-from-url "http://tromey.com/elpa/package-install.el")

(require '00_init-macro)
(add-to-list 'load-path "~/.emacs.d/plugins/ruby/")

;; ruby-mode
(when (autoload-if-found 'ruby-mode "ruby-mode"
                         "Mode for editing ruby source files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

  (defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)
  (autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
  (autoload 'inf-ruby-setup-keybindings "inf-ruby" "" t)
  (eval-after-load 'ruby-mode
    (progn
      ;; ruby-block
      (req ruby-block)
      (ruby-block-mode t)
      ;; ミニバッファに表示し, かつ, オーバレイする.
      (setq ruby-block-highlight-toggle t)
      (defun ruby-mode-hook-ruby-block()
        (ruby-block-mode t))
      (req ruby-electric)
      ;; set ruby-mode indent
      (setq ruby-indent-level 2)
      (setq ruby-indent-tabs-mode nil)
      )))

(add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings)

;; ruby-electric
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode 1)))
(setq ruby-electric-expand-delimiters-list nil)

(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

;; ;;;; rubydb
;; (autoload 'rubydb "rubydb3x"
;;   "run rubydb on program file in buffer *gud-file*.
;; the directory containing file becomes the initial working directory
;; and source-file directory for your debugger." t)

;; ;; rails
;; (defun try-complete-abbrev (old)
;;   (if (expand-abbrev) t nil))
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))
;; (setq rails-use-mongrel t)
;; (req cl)
;; (req rails)

(add-hook 'ruby-mode-hook 'ruby-mode-hook-ruby-block)

(require 'ruby-end)

(add-to-list 'auto-mode-alist '(".erb$" . rhtml-mode))
(defun html-setting ()
  (setq c-basic-offset 1
        tab-width 1))
(add-hook 'rhtml-mode-hook 'html-setting)
(require 'rhtml-mode)
(require 'rinari)
(add-hook 'rhtml-mode-hook
    (lambda () (rinari-launch)))

;;; 50_init-ruby-mode.el ends here
