
(defun temporary-repairs-yas-keybind ()
  '(lambda ()
	 (interactive)
	 (insert "k")))
(define-key emacs-lisp-mode-map (kbd "k") 'temporary-repairs-yas-keybind)
