;;shell-pop***************************
;;@see http://d.hatena.ne.jp/kyagi/20090601
;;http://sakito.jp/emacs/emacsshell.html
(req shell-pop)
(add-to-list 'shell-pop-internal-mode-list '("multi-term" "*terminal<1>*" '(lambda () (multi-term))))
(shell-pop-set-internal-mode "multi-term")


;;shell-toggle************************
;;@see http://gihyo.jp/admin/serial/01/ubuntu-recipe/0038
(load-library "~/.emacs.d/elisp/shell-toggle-pathed.el")
(autoload 'shell-toggle "shell-toggle"
 "Toggles between the *shell* buffer and whatever buffer you are editing."
 t)
(autoload 'shell-toggle-cd "shell-toggle"
 "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
(global-set-key "\C-ct" 'shell-toggle)
(global-set-key "\C-cd" 'shell-toggle-cd)


;;shell-command*******************
;;auto-install-from emacswiki shell-command.el
;; (req shell-command)
;; (shell-command-completion-mode)




;; shell
(when (req shell-history nil t)
  (when (req anything-complete nil t)
    (add-hook 'shell-mode-hook
	      (lambda ()
		(define-key shell-mode-map (kbd "C-r") 'anything-complete-shell-history)))
    
    (use-anything-show-completion 'anything-complete-shell-history
				  '(length anything-c-source-complete-shell-history))))

;; ansi-colorでエスケープシーケンスをfontifyする設定
;;@see http://d.hatena.ne.jp/rubikitch/20081102/1225601754
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(when (req shell-pop nil t)
  (setq shell-pop-window-height 60) ; percentage for shell-buffer window height
  (define-key global-map [(super t)] 'shell-pop))


;;multi-term***************************
;;@see http://sakito.jp/emacs/emacsshell.html
(req multi-term)
(setq multi-term-program shell-file-name)

(add-to-list 'term-unbind-key-list '"M-x")

(global-set-key (kbd "C-c t") '(lambda ()
				 (interactive)
				 (multi-term)))