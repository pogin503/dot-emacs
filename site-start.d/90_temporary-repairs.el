;;; 90_temporary-repairs --- 90_temporary-repairs
;; This program is free software
;;; Commentary:
;;; Code:
;; (defun temporary-repairs-yas-keybind ()
;;   '(lambda ()
;; 	 (interactive)
;; 	 (insert "k")))
;;(defvar test-map (make-sparse-keymap))

;; (define-key test-map (kbd "k") '(lambda () (insert "k")))

;;
;; (define-key emacs-lisp-mode-map (kbd "k")
;;   '(lambda ()
;; 	 (interactive)
;; 	 (insert "k")))

;; for oyayubi shift
(global-set-key (kbd "M-,") #'(lambda () (interactive) (insert "、")))

(provide '90_temporary-repairs)
;;; 90_temporary-repairs ends here
