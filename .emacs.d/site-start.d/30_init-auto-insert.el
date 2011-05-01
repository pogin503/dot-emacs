;;@see http://ja.green.xrea.jp/emacs/autoinsert-mode
;;@see 
;;(require 'autoinsert)
;;(require 'cl)
;;(add-hook 'before-save-hook 'time-stamp)
;;(auto-insert-mode t)
;;(setq auto-insert-directory "~/etc/autoinsert/")
;; (setq auto-insert-alist
;;       (nconc '(("\\.lisp$" . ["template.lisp" my-template])
;; 		("\\.asd$" . ["template.asd" "template.lisp"
;; 			      (lambda () (my-template-package
;; 					  (my-file-body-name
;; 					   (file-name-nondirectory
;; 					    (buffer-file-name)))))
;; 			      my-template])
;; 		("\\.hs$" . ["template.hs" my-template])
;; 		("\\.r$" . ["template.r" my-template])
;; 		("\\.scm" . ["template.scm"
;; 			     (lambda() (my-template-exec "/usr/local/bin/gosh"))
;; 			     my-template])
;; 		("\\.sh$" . ["template.sh"
;; 			     (lambda() (my-template-exec "/bin/sh"))
;; 			     my-template])
;; 		("\\.py$" . ["template.sh"
;; 			     (lambda() (my-template-exec "/usr/bin/python"))
;; 			     my-template])
;; 		("\\.rb$" . ["template.sh"
;; 			     (lambda() (my-template-exec "/usr/bin/ruby"))
;; 			     my-template])
;; 		("\\.cpp$" . ["template.cpp" my-template])
;; 		("\\.h$"   . ["template.h" my-template])
;; 		(xml-mode . "xml-insert.xml")
;;                 (texinfo-mode . "texinfo.texi")
;; 		)
;; 	     auto-insert-alist))

;(add-hook ‘find-file-hooks ‘auto-insert)

;; (defvar template-replacements-alists
;;   ‘(("%file%" . (lambda()(file-name-nondirectory (buffer-file-name))))
;;     ("%name%" . user-full-name)
;;     ("%mail%" . (lambda()(identity user-mail-address)))
;;     ("%cyear%" . (lambda()(substring (current-time-string) -4)))
;;     ("%license%" . (lambda()(read-from-minibuffer "License: ")))
;;     ("%bdesc%" . (lambda()(read-from-minibuffer "Brief dscription: ")))))

;; (defun my-file-body-name (file-name)
;;   (substring file-name 0 (position 46 file-name)))

;; (defmacro defreplace (name replace-string)
;;   `(defun ,name (str)
;;      (goto-char (point-min))
;;      (replace-string ,replace-string str)))

;; (defreplace my-template-exec "%exec%")
;; (defreplace my-template-package "%package%")

;; (defun my-template ()
;;   (time-stamp)
;;   (mapc #’(lambda(c)
;; 	    (progn
;; 	      (goto-char (point-min))
;; 	      (replace-string (car c) (funcall (cdr c)) nil)))
;; 	template-replacements-alists)
;;   (goto-char (point-max))
;;   (message "done."))

;(add-hook 'find-file-not-found-hooks 'auto-insert)
