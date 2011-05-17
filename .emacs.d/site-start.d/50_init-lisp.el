(add-hook 'lisp-mode-hook (lambda ()
                            (show-paren-mode t)))
(add-hook 'after-change-major-mode-hook 
	  (lambda ()
	    (show-paren-mode t)))
(add-hook 'lisp-mode-hook
	  (lambda ()
	    (set (make-local-variable 'lisp-indent-function)
		 'common-lisp-indent-function)))