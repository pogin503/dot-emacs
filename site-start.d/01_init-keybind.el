
;;original key-bind
(global-set-key (kbd "C-8")
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

;; (global-set-key "\M-w" 'clipboard-kill-ring-save)  ; クリップボードにコピー
;; (global-set-key "\C-w" 'clipboard-kill-region)     ; 切り取ってクリップボードへ

(global-set-key (kbd "C-M-k") 
  (lambda ()
    (interactive)
    (kill-buffer (current-buffer))))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)

(global-set-key (kbd "C-h") 'delete-backward-char)
;; (keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-m") 'newline-and-indent)
