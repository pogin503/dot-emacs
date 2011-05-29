;(auto-install-from-url "http://tromey.com/elpa/package-install.el")

(add-to-list 'load-path "~/.emacs.d/plugins/ruby/")

;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda () (inf-ruby-keys)))

;; ruby-electric
(req ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

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

;; ruby-block
(req ruby-block)
(ruby-block-mode t)
;; ミニバッファに表示し, かつ, オーバレイする.
(setq ruby-block-highlight-toggle t)
(defun ruby-mode-hook-ruby-block()
  (ruby-block-mode t))
(add-hook 'ruby-mode-hook 'ruby-mode-hook-ruby-block)

(defun execute-ruby ()
  (interactive)
  (let (buf)
    (save-excursion
      (setq buf (get-buffer-create "*result ruby execution*"))
      (mark-whole-buffer)
      (call-process-region (region-beginning) (region-end) "ruby" nil buf nil)
      (display-buffer buf))))

(define-key ruby-mode-map (kbd "C-7")'execute-ruby)

;; set ruby-mode indent
(setq ruby-indent-level 2)
(setq ruby-indent-tabs-mode nil)