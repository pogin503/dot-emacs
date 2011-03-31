;;shell-pop***************************
;;@see http://d.hatena.ne.jp/kyagi/20090601
;;http://sakito.jp/emacs/emacsshell.html
(require 'shell-pop)
(add-to-list 'shell-pop-internal-mode-list '("multi-term" "*terminal<1>*" '(lambda () (multi-term))))
(shell-pop-set-internal-mode "multi-term")


;;shell-toggle************************
;;@see http://gihyo.jp/admin/serial/01/ubuntu-recipe/0038
;(load-library "~/.emacs.d/elisp/shell-toggle-pathed.el")
 ; (autoload 'shell-toggle "shell-toggle"
 ;  "Toggles between the *shell* buffer and whatever buffer you are editing."
 ;  t)
 ; (autoload 'shell-toggle-cd "shell-toggle"
 ;  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
 ; (global-set-key "\C-ct" 'shell-toggle)
 ; (global-set-key "\C-cd" 'shell-toggle-cd)


;;shell-command*******************
;;auto-install-from emacswiki shell-command.el
(require 'shell-command)
(shell-command-completion-mode)



;;@see http://d.hatena.ne.jp/tomoya/20090601/1243817036
;; eshell
(when (require 'eshell-auto nil t)
  (add-hook 'eshell-mode-hook
	    (lambda () 
	      (define-key eshell-mode-map (kbd "C-a") 'eshell-bol)
	      (define-key eshell-mode-map (kbd "C-r") 'eshell-isearch-backward)))

  (when (require 'pcmpl-auto nil t)
    (when (require 'pcmpl-ssh nil t)
      (add-hook 'eshell-mode-hook 'pcomplete-shell-setup)))

  (setq eshell-cmpl-ignore-case t)	; 補完時に大文字小文字を区別しない
  (setq eshell-glob-include-dot-dot nil) ; ../ を * でマッチさせない
  (setq eshell-ask-to-save-history (quote always)) ; 確認なしでヒストリ保存
  (setq eshell-history-file-name "~/.zsh_history") ; zsh のヒストリと共有
  (setq eshell-history-size 100000)				   ; ヒストリサイズ
  (setq eshell-hist-ignoredups t))		; ヒストリの重複を無視

;; shell
(when (require 'shell-history nil t)
  (when (require 'anything-complete nil t)
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

(when (require 'shell-pop nil t)
  (setq shell-pop-window-height 60) ; percentage for shell-buffer window height
  (define-key global-map [(super t)] 'shell-pop))


;;multi-term***************************
;;@see http://sakito.jp/emacs/emacsshell.html
(require 'multi-term)
(setq multi-term-program shell-file-name)

(add-to-list 'term-unbind-key-list '"M-x")

(global-set-key (kbd "C-c t") '(lambda ()
                                (interactive)
                                (multi-term)))