;; 50_init-coq-mode.el

;; Windows Setting
(when run-windows
  (progn
    (add-to-list 'exec-path "C:/Program Files (x86)/Coq/bin")
    (load-file
     (concat dropbox-directory
             "/プログラミング/ProofGeneral-4.1/ProofGeneral-4.1/generic/proof-site.el"))))

;; 要 $ yaourt -S proofgeneral
(when (file-exists-p
       "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
  (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el"))

(lazyload (coq-mode) "coq"
          ;; set auto-complete-mode
          (defun my-ac-coq-mode ()
            (setq ac-sources '(ac-source-words-in-same-mode-buffers
                               ac-source-dictionary)))
          (add-to-list 'ac-modes 'coq-mode)
          )

(add-hook 'coq-mode-hook 'auto-complete-mode)
(add-hook 'coq-mode-hook 'my-ac-coq-mode)
