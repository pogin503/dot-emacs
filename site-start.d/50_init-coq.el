;;; 50_init-coq --- 50_init-coq
;; This program is free software
;;; Commentary:
;;; Code:

(require 'f)
(require 'leaf)
;; (when (f-exists? "/usr/local/opt/coq/lib/emacs/site-lisp")
;;   (progn
;; 	(setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;; 	(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
;; 	(custom-set-variables
;; 	 '(coq-prog-name "/usr/local/bin/coqtop.opt"))
;; 	))

;; (if run-windows
;;     ;; run Windows
;;     (if run-windows-x64
;; 		(add-to-list 'exec-path "C:/Program Files (x86)/Coq/bin")
;;       (add-to-list 'exec-path "C:/Program Files/Coq/bin"))
;;   ;; run other OS
;;   (let ((file1 (concat dropbox-directory
;;                       "eworkspace/ProofGeneral-4.2/ProofGeneral-4.2/generic/proof-site.el"))
;;         (file2 (concat dropbox-directory
;;                  "eworkspace/ssreflect-1.4/pg-ssr.el")))
;;     (if (f-exists? file1)
;;         (load-file file1)
;;       (error "Please install ProofGeneral"))
;;     (if (f-exists? file2)
;;         (load-file file2)
;;       (error "Please install Ssreflect")))
;;   )

(leaf company
  :ensure t)

(leaf proof-general
  :ensure t
  :init
  (custom-set-variables
   '(coq-prog-name "/usr/local/bin/coqtop")
   '(proof-three-window-enable t))
  (setq proof-splash-enable nil)
  )

(leaf company-coq
  :ensure t)

;; (custom-set-variables
;;  '(coq-prog-name "/usr/local/bin/coqtop")
;;  '(package-selected-packages
;;    (quote
;;     (company-coq company proof-general)))
;;  '(proof-three-window-enable t))

(add-hook 'coq-mode-hook 'company-coq-mode)

(provide '50_init-coq)
;;; 50_init-coq ends here
