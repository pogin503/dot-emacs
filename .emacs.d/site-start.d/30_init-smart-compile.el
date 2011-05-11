(require 'smart-compile)
(defvar smart-compile-alist '(
  ("\\.c\\'"          . "gcc -O2 %f -lm -o %n")
  ("\\.[Cc]+[Pp]*\\'" . "g++ -O2 %f -lm -o %n")
  ("\\.java\\'"       . "javac %f")
  ("\\.f90\\'"        . "gfortran %f -o %n")
  ("\\.[Ff]\\'"       . "gfortran %f -o %n")
  ("\\.tex\\'"        . (tex-file))
  ("\\.pl\\'"         . "perl -cw %f")
  (emacs-lisp-mode    . (emacs-lisp-byte-compile))
) "...")

(global-set-key "\C-cc" 'smart-compile)
(define-key menu-bar-tools-menu [compile] '("Compile..." . smart-compile))