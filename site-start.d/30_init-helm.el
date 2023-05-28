;;; 30_init-helm.el --- helm conf
;;; Commentary:
;;; Code:

(require 'helm-config)
(require '00_init-vars)

;; (require 'helm-descbinds)
;; (helm-descbinds-mode)
;; (helm-mode 1)

;;; 処理を変更したいコマンドをリストに登録していく
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
(use-package helm
  :config
  (helm-autoresize-mode 1))

(custom-set-variables
  '(helm-M-x-always-save-history t)
  '(helm-adaptive-history-file (locate-user-emacs-file ".cache/helm-adaptive-history")))

(use-package helm-gtags
  :hook (
         (c-mode . helm-gtags-mode)
         (c++-mode . helm-gtags-mode)
         (asm-mode . helm-gtags-mode)
         )
  :config
  ;; customize
  (custom-set-variables
   '(helm-gtags-path-style 'relative)
   '(helm-gtags-ignore-case t)
   '(helm-gtags-auto-update t)))


(provide '30_init-helm)
;;; 30_init-helm.el ends here
