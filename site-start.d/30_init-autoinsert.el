;;; 30_init-auto-insert.el --- for autoinsert conf  -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;; Code:
;; @see http://ja.green.xrea.jp/emacs/autoinsert-mode
(require 'autoinsert)

(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory  (locate-user-emacs-file "etc/autoinsert"))
(setq auto-insert-alist
      (nconc '(
               ("\\.pl$"   . ["perl.pl"       my-template])
               ("\\.agda$" . ["template.agda" my-template])
               ("\\.c$"    . ["template.c"    my-template])
               ("\\.cpp$"  . ["template.cpp"  my-template])
               ("\\.dats$" . ["template.dats" my-template])
               ("\\.h$"    . ["template.h"    my-template])
               ("\\.hs$"   . ["template.hs"   my-template])
               ("\\.html$" . ["template.html" my-template])
               ("\\.php$"  . ["template.php"  my-template])
               ("\\.py$"   . ["template.py"   my-template])
               ("\\.rb$"   . ["template.rb"   my-template])
               ("\\.rs$"   . ["template.rs"   my-template])
               ("\\.sh$"   . ["template.sh"
                            (lambda() (my-template-exec "#!/usr/bin/env bash"))
                            my-template])
               ("\\.editorconfig$"    . ["template.editorconfig" my-template])
               ("\\.dir-locals\\.el") . ["template.dir-locals.el" my-template])
             auto-insert-alist))

(use-package s)
(defconst template-replacements-alists
  '(
    ("%file%" . (lambda()(file-name-nondirectory (buffer-file-name))))
    ("%name%" . user-full-name)
    ("%mail%" . (lambda()(identity user-mail-address)))
    ("%file-without-ext%" . (lambda ()
                              (file-name-sans-extension
                               (file-name-nondirectory (buffer-file-name)))))
    ("%include-guard%"    . (lambda ()
                              (format "__%s__"
                                      (upcase
                                       (file-name-sans-extension
                                        (file-name-nondirectory buffer-file-name))))))
    ("%rbclass%" . (lambda ()
                     (s-replace "_" ""
                      (capitalize
                       (file-name-sans-extension
                        (file-name-nondirectory (buffer-file-name)))))
))
    ))

(defun my-file-body-name (file-name)
  (substring file-name 0 (cl-position 46 file-name)))

(defmacro defreplace (name arg_replace-string)
  `(defun ,name (str)
     (goto-char (point-min))
     (replace-string ,arg_replace-string str)))

(defreplace my-template-exec "%exec%")

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

(provide '30_init-autoinsert)
;;; 30_init-autoinsert.el ends here
