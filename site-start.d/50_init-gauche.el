;;; 50_init-gauche --- 50_init-gauche
;; This program is free software
;;; Commentary:
;;; Code:

;;@see http://d.hatena.ne.jp/YOMOGItaro/20100501/1272674144
;;gauche
(modify-coding-system-alist 'process "gosh" '(utf-8 . utf-8))

(setq scheme-program-name "gosh -i")
;; (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
;; (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(require '00_init-hanbetu)
;;inferior-gauche-mode
(if (equal system-type run-linux)
	(setq auto-mode-alist
		  (cons '("\\.scm$" . inferior-gauche-mode) auto-mode-alist)))
;(setq default-major-mode 'inferior-gauche-mode)
;(inferior-gauche-mode)

(lazyload (inferior-gauche-mode) "inferior-gauche-mode"
          (req inferior-gauche)
          (defun scheme-other-window ()
            "Run scheme on other window"
            (interactive)
            (switch-to-buffer-other-window
             (get-buffer-create "*scheme*"))
            (run-scheme scheme-program-name))
          (define-key global-map "\C-cs" 'scheme-other-window)
          )
(provide '50_init-gauche)
;;; 50_init-gauche ends here
