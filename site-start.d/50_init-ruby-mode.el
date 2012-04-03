;(auto-install-from-url "http://tromey.com/elpa/package-install.el")

(add-to-list 'load-path "~/.emacs.d/plugins/ruby/")

;; ruby-mode
(when (autoload-if-found 'ruby-mode "ruby-mode"
                         "Mode for editing ruby source files" t)
  (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

  (when (and (autoload-if-found 'run-ruby "inf-ruby"
                                "Run an inferior Ruby process" t)
             (autoload-if-found 'inf-ruby-keys "inf-ruby"
                       "Set local key defs for inf-ruby in ruby-mode" t))
    (add-hook 'ruby-mode-hook
              '(lambda () (inf-ruby-keys))))

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
      ;; (defun execute-ruby ()
      ;;   (interactive)
      ;;   (save-excursion
      ;;     (let ((buf (get-buffer-create "*result ruby execution*")))
      ;;       (mark-whole-buffer)
      ;;       (call-process-region
      ;;        (region-beginning) (region-end) "ruby" nil buf nil)
      ;;       (display-buffer buf))))

      ;; (define-key ruby-mode-map (kbd "C-7")'execute-ruby)

      )))

;; ruby-electric
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

(add-hook 'ruby-mode-hook 'ruby-mode-hook-ruby-block)
