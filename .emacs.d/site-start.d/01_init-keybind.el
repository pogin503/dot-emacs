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
;;original 
(global-set-key "\M-w" 'clipboard-kill-ring-save)  ; クリップボードにコピー
(global-set-key "\C-w" 'clipboard-kill-region)     ; 切り取ってクリップボードへ

(define-key global-map (kbd "C-M-k") 
  (lambda ()
    (interactive)
    (kill-buffer buffer-file-name)))
