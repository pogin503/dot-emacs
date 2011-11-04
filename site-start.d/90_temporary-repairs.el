
;; (defun temporary-repairs-yas-keybind ()
;;   '(lambda ()
;; 	 (interactive)
;; 	 (insert "k")))
;;(defvar test-map (make-sparse-keymap))

;; (define-key test-map (kbd "k") '(lambda () (insert "k")))
(define-key emacs-lisp-mode-map (kbd "k") 
  '(lambda ()
	 (interactive)
	 (insert "k")))
