
(when run-windows
  (progn
    (add-to-list 'exec-path "C:/Program Files (x86)/Coq/bin")
    (load-file (concat dropbox-directory "/プログラミング/ProofGeneral-4.1/ProofGeneral-4.1/generic/proof-site.el"))))

(when (file-exists-p
       "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
  (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el"))
