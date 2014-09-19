;;; 30_init-auto-insert.el --- for autoinsert conf
;;; Commentary:
;;; Code:
;; @see http://ja.green.xrea.jp/emacs/autoinsert-mode
(require 'autoinsert)

;; (add-hook 'before-save-hook 'time-stamp)
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory  "~/.emacs.d/etc/autoinsert")
(setq auto-insert-alist
      (nconc '(
               ;; ("\\.lisp$" . ["template.lisp" my-template])
;; 		("\\.asd$" . ["template.asd" "template.lisp"
;; 			      (lambda () (my-template-package
;; 					  (my-file-body-name
;; 					   (file-name-nondirectory
;; 					    (buffer-file-name)))))
;; 			      my-template])
	        ("\\.hs" . ["template.hs" my-template])
            ("\\.agda" . ["template.agda" my-template])
	        ("\\.rb" . ["template.rb" my-template])
		;; 		("\\.r$" . ["template.r" my-template])
;; 		("\\.scm" . ["template.scm"
;; 			     (lambda() (my-template-exec "/usr/local/bin/gosh"))
;; 			     my-template])
		("\\.sh$" . ["template.sh"
                     (lambda() (my-template-exec "#!/bin/sh"))
                     my-template])
		("\\.py$" . ["template.py" my-template])
;; 			     my-template])
;; 		("\\.rb$" . ["template.sh"
;; 			     (lambda() (my-template-exec "/usr/bin/ruby"))
;; 			     my-template])
		("\\.c$" . ["template.c" my-template])
		("\\.cpp$" . ["template.cpp" my-template])
		("\\.h$"   . ["template.h" my-template])
;; 		(xml-mode . "xml-insert.xml")
;;                 (texinfo-mode . "texinfo.texi")
		)
	    auto-insert-alist))

(defvar template-replacements-alists
  '(
    ("%file%" . (lambda()(file-name-nondirectory (buffer-file-name))))
    ("%name%" . user-full-name)
    ("%mail%" . (lambda()(identity user-mail-address)))
    ;; ("%cyear%" . (lambda()(substring (current-time-string) -4)))
    ;; ("%license%" . (lambda()(read-from-minibuffer "License: ")))
    ;; ("%bdesc%" . (lambda()(read-from-minibuffer "Brief dscription: ")))
    ("%file-without-ext%" . (lambda ()
                              (file-name-sans-extension
                               (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%"    . (lambda ()
                              (format "__%s__"
                                      (upcase
                                       (file-name-sans-extension
                                        (file-name-nondirectory buffer-file-name))))))
    ("%rbclass%" . (lambda () (capitalize
			   (file-name-sans-extension
			    (file-name-nondirectory (buffer-file-name))))))
    ))

(eval-when-compile
  (require 'cl))

(defun my-file-body-name (file-name)
  (substring file-name 0 (position 46 file-name)))

(defmacro defreplace (name arg_replace-string)
  `(defun ,name (str)
     (goto-char (point-min))
     (replace-string ,arg_replace-string str)))

(defreplace my-template-exec "%exec%")
;; (defreplace my-template-package "%package%")

(defun my-template ()
  "Templateの挿入."
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))

;;; 30_init-auto-insert.el ends here
