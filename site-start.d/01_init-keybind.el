;;; 01_init-keybind.el --- keybind conf
;;; Commentary:
;;; Code:
;;original key-bind

(defun my-load-current-buffer ()
  (interactive)
  (save-buffer)
  ;;  Tell me about all errors
  (if (boundp 'debug-ignored-errors)
      (setq debug-ignored-errors nil))
  (if (equal debug-on-error nil)
      (setq debug-on-error t))
  (load-file buffer-file-name)
  (message "load %S succeeded!" (current-buffer)))
(global-set-key (kbd "C-8") 'my-load-current-buffer)
(global-set-key (kbd "s-8") 'my-load-current-buffer)

;;original end

;; (global-set-key "\M-w" 'clipboard-kill-ring-save)  ; クリップボードにコピー
;; (global-set-key "\C-w" 'clipboard-kill-region)     ; 切り取ってクリップボードへ

(global-set-key (kbd "C-M-k")
  (lambda ()
    (interactive)
    (kill-buffer (current-buffer))))

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-z") 'redo)
(global-set-key (kbd "M-S-z") 'redo)
(global-set-key (kbd "s-<right>") 'right-word)
(global-set-key (kbd "s-<left>") 'left-word)
(global-set-key (kbd "s-<up>") 'backward-paragraph)
(global-set-key (kbd "s-<down>") 'forward-paragraph)
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)
(global-set-key (kbd "s-w") 'kill-ring-save)

(define-key global-map [?¥] [?\\])  ;; ¥の代わりにバックスラッシュを入力する

;; (global-set-key (kbd "C-h") 'delete-backward-char)
(keyboard-translate ?\C-h ?\C-?)

;; (global-set-key (kbd "C-m") 'newline-and-indent)

;; (global-set-key (kbd "C-<tab>") 'indent-for-tab-command)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)


(global-set-key (kbd "M-o") 'edit-next-line)
;; (global-set-key (kbd "s-RET") 'edit-next-line)
;; (global-set-key (kbd "s-<RET>") 'edit-next-line)
(global-set-key (kbd "S-<return>") 'edit-next-line)
(global-set-key (kbd "M-O") 'edit-previous-line)
(global-set-key (kbd "C-S-<return>") 'edit-previous-line)


(global-set-key (kbd "M-l") 'forward-match-char)
(global-set-key (kbd "M-L") 'backward-match-char)

(global-set-key (kbd "C-x C-c") 'anything-execute-extended-command)

;; I never use C-x C-c
(defalias 'exit 'save-buffers-kill-emacs)

(global-set-key (kbd "C-x C-z") 'nil)

(global-set-key (kbd "C-x C-o") 'other-window)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
;;; 01_init-keybind.el ends here
