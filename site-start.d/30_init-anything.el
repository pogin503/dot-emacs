;;; 30_init-anything.el --- anything conf
;;; Commentary:
;;; Code:
(eval-when-compile
  (require '00_init-macro)
  (require 'cl))

;; (when (autoload-if-found 'anything-execute-extended-command "anything" nil t)
;;   (global-set-key (kbd "M-x") 'anything-execute-extended-command))

(lazyload (anything-execute-extended-command
           anything) "anything"
          ;;anything
          (setq org-directory "")
          (req anything-startup)
          ;; (req anything-config)
          ;; (req recentf)
          ;; (define-key global-map (kbd "M-l") 'anything)
          ;; (setq recentf-max-saved-items 3000)
          ;; (recentf-mode t)
          ;; (setq anything-sources
          ;;       '(anything-c-source-buffers+
          ;;         anything-c-source-colors
          ;; 	anything-c-source-recentf
          ;;         anything-c-source-bookmarks
          ;;         anything-c-source-file-cache
          ;;         anything-c-source-man-pages
          ;;         anything-c-source-emacs-variable-at-point
          ;;         anything-c-source-emacs-function-at-point
          ;;         anything-c-source-file-name-history
          ;;         anything-c-source-anything-grep-fallback
          ;;         anything-c-source-anything-google-fallback
          ;;         anything-c-source-emacs-commands
          ;;         anything-c-source-emacs-functions
          ;;         anything-c-source-files-in-current-dir+
          ;;        ))
	  (require 'mylib)
	  (font-family-list)
	  )

(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-mini)

(require 'helm-descbinds)
(helm-descbinds-mode)
;; (helm-mode 1)

(provide '30_init-anything)
;;; 30_init-anything.el ends here
