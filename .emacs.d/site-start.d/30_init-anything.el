;;anything
(require 'anything)
(require 'anything-startup)
(require 'anything-config)
(require 'recentf)
;(add-to-list 'anything-sources 'anything-c-source-emacs-commands)
(define-key global-map (kbd "M-l") 'anything)
(setq recentf-max-saved-items 3000)
(recentf-mode t)
(setq anything-sources
      '(anything-c-source-buffers+
;        anything-c-source-colors
        anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-file-cache
        anything-c-source-man-pages
;        anything-c-source-emacs-variable-at-point
;        anything-c-source-emacs-function-at-point
        anything-c-source-file-name-history
;        anything-c-source-anything-grep-fallback
;        anything-c-source-anything-google-fallback
        anything-c-source-emacs-commands
        anything-c-source-emacs-functions
        anything-c-source-files-in-current-dir+
        ))
