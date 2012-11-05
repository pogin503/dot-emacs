;; 50_init-coq-mode.el

;; Windows Setting
(when run-windows
  (progn
    (if run-windows-x64
        (add-to-list 'exec-path "C:/Program Files (x86)/Coq/bin")
      (add-to-list 'exec-path "C:/Program Files/Coq/bin"))
    (load-file
     (concat dropbox-directory
             "/eworkspace/ProofGeneral-4.1/ProofGeneral-4.1/generic/proof-site.el"))))

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

;; coq-mode-hook
(add-hook 'coq-mode-hook 'auto-complete-mode)
(add-hook 'coq-mode-hook 'my-ac-coq-mode)
