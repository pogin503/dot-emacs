;; 50_init-coq-mode.el

;; Windows Setting
(if run-windows
    (if run-windows-x64
        (add-to-list 'exec-path "C:/Program Files (x86)/Coq/bin")
      (add-to-list 'exec-path "C:/Program Files/Coq/bin")))
(load-file
 (concat dropbox-directory
         "/eworkspace/ProofGeneral-4.1/ProofGeneral-4.1/generic/proof-site.el"))

;; 要 $ yaourt -S proofgeneral
;; (when (file-exists-p
;;        "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")
;;   (load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el"))

(defun my-ac-coq-mode ()
  (setq ac-sources '(ac-source-words-in-same-mode-buffers
                     ac-source-dictionary)))

(lazyload (coq-mode) "proof-site"
          ;; set auto-complete-mode
          (require 'proof-site)
          (add-to-list 'ac-modes 'coq-mode)
          )

;; coq-mode-hook
(add-hook 'coq-mode-hook 'auto-complete-mode)
(add-hook 'coq-mode-hook 'my-ac-coq-mode)
(add-to-list 'auto-mode-alist '(".v$" . coq-mode))
