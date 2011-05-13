;;original key-bind
(define-key global-map (kbd "C-8")
  (lambda ()
    (interactive)
    (save-buffer)
;;  Tell me about all errors
    (if (boundp 'debug-ignored-errors)
	(setq debug-ignored-errors nil))
    (if (equal debug-on-error nil)
	(setq debug-on-error t))
    (load-file buffer-file-name)
    (message "load %S succeeded!" (current-buffer))))
;;original end

(global-set-key "\M-w" 'clipboard-kill-ring-save)  ; クリップボードにコピー
(global-set-key "\C-w" 'clipboard-kill-region)     ; 切り取ってクリップボードへ

(define-key global-map (kbd "C-M-k") 
  (lambda ()
    (interactive)
    (kill-buffer buffer-file-name)))

;;@see EmacsWiki WishList
(defun copy (beg end)
  "Copy region into clipboard"
  (interactive "r")
  (if mark-active
      (progn
        (x-set-selection 'CLIPBOARD (buffer-substring beg end))
        (setq mark-active nil)
        (message "Marked text copied"))
    (progn
      (x-set-selection 'CLIPBOARD (buffer-substring line-beginning-position line-end-position))
      (setq mark-active nil)
      (message "Current line is copied"))))

(defun cut (beg end)
  "Copy region into clipboard and kill"
  (interactive "r")
  (if mark-active
      (progn
        (x-set-selection 'CLIPBOARD (buffer-substring beg end))
        (delete-region beg end)
        (setq mark-active nil)
        (message "Marked text cut"))
    (progn
      (x-set-selection 'CLIPBOARD (buffer-substring line-beginning-position line-end-position))
      (delete-region line-beginning-position line-end-position)
      (setq mark-active nil)
      (message "Current line is cut"))))

(defun paste (beg end)
  "Paste contents of clipboard into point"
  (interactive "r")
  (if mark-active
      (delete-region beg end))
  (insert (x-selection 'CLIPBOARD)))


;;end EmacsWiki WishList

;(global-set-key "\M-w" 
;		(lambda ()
;		  (interactive)
;		  (clipboard-kill-ring-save)))
;		  (progn
;		    (copy)
;		    (clipboard-kill-ring-save))))  ; クリップボードにコピー
;(global-set-key "\C-w"
;		(lambda ()
;		  (interactive)
;		  (clipboard-kill-region)))
;		  (progn
;		    (cut)
;		    (clipboard-kill-region))))  ; クリップボードにコピー

(global-set-key (kbd "C-x C-e") 'eval-last-sexp-popup)

()