;;; 30_init-anything.el --- anything conf
;;; Commentary:
;;; Code:

(eval-when-compile
  (require '00_init-macro)
  (require 'cl))

;; TABで任意補完。選択肢が出てきたらC-nやC-pで上下移動してから決定することも可能
;; (define-key helm-c-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

(require 'helm-config)

(global-set-key (kbd "M-]") 'helm-mini)
(global-set-key (kbd "s-]") 'helm-mini)

(require 'helm-descbinds)
(helm-descbinds-mode)
(helm-mode 1)

(custom-set-variables
  '(helm-M-x-always-save-history t)
  '(helm-adaptive-history-file (locate-user-emacs-file "cache/helm-adaptive-history")))

(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-x") 'execute-extended-command)

(provide '30_init-anything)
;;; 30_init-anything.el ends here
