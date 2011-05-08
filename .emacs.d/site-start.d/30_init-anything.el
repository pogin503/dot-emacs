;;anything
(require 'anything-startup)
;(require 'anything-config)
;(require 'recentf)
;(define-key global-map (kbd "M-l") 'anything)
(setq recentf-max-saved-items 3000)
;(recentf-mode t)
;(setq anything-sources
;      '(anything-c-source-buffers+
;        anything-c-source-colors
;	anything-c-source-recentf
;        anything-c-source-bookmarks
;        anything-c-source-file-cache
;        anything-c-source-man-pages
;        anything-c-source-emacs-variable-at-point
;        anything-c-source-emacs-function-at-point
;        anything-c-source-file-name-history
;        anything-c-source-anything-grep-fallback
;        anything-c-source-anything-google-fallback
;        anything-c-source-emacs-commands
;        anything-c-source-emacs-functions
;        anything-c-source-files-in-current-dir+
;       ))

;;anything-font-families
(defun anything-font-families ()
  "Preconfigured `anything' for font family."
  (interactive)
  (flet ((anything-mp-highlight-match () nil))
    (anything-other-buffer
     '(anything-c-source-font-families)
     "*anything font families*")))

(defun anything-font-families-create-buffer ()
  (with-current-buffer
      (get-buffer-create "*Fonts*")
    (loop for family in (sort (delete-duplicates (font-family-list)) 'string<)
          do (insert
              (propertize (concat family "\n")
                          'font-lock-face
                          (list :family family :height 2.0 :weight 'bold))))
    (font-lock-mode 1)))

(defvar anything-c-source-font-families
      '((name . "Fonts")
        (init lambda ()
              (unless (anything-candidate-buffer)
                (save-window-excursion
                  (anything-font-families-create-buffer))
                (anything-candidate-buffer
                 (get-buffer "*Fonts*"))))
        (candidates-in-buffer)
        (get-line . buffer-substring)
        (action
         ("Copy Name" lambda
          (candidate)
          (kill-new candidate))
         ("Insert Name" lambda
          (candidate)
          (with-current-buffer anything-current-buffer
            (insert candidate))))))

(font-family-list)
;;end anything-font-families
