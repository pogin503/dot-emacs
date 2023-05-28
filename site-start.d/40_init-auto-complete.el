;;; 40_init-auto-complete --- auto-complete conf
;;; Commentary:
;; @see http://cx4a.org/software/auto-complete/manual.ja.html
;;; Code:

;; (use-package auto-complete
;;   :config
;;   (require 'auto-complete)
;;   (require 'auto-complete-config)

;;   (setq ac-comphist-file (locate-user-emacs-file  "etc/ac-comphist.dat"))
;;   (ac-config-default)
;;   ;; (global-auto-complete-mode t)
;;   (setq-default ac-use-comphist t)
;;   (setq-default ac-auto-start 4
;;                 ac-auto-show-menu 1.0)

;;   (dolist (hook (list
;;                  'html-mode-hook
;;                  'sgml-mode-hook
;;                  'nxml-mode-hook
;;                  ))
;;     (add-hook hook 'auto-complete-mode))

;;   ;; coq
;;   (defun my-ac-coq-mode ()
;;     "Set coq ac-source."
;;     (setq ac-sources '(ac-source-words-in-same-mode-buffers
;;                        ac-source-dictionary))))

(use-package company
  :config
  (global-company-mode)
  (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
  ;; Trigger completion immediately.
  ;; (setq company-idle-delay 0)

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  )

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))




(use-package company-tabnine
  :config
  (add-to-list 'company-backends #'company-tabnine))

(provide '40_init-auto-complete)
;;; 40_init-auto-complete ends here
