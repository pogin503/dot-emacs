;;; 50_init-shell --- 50_init-shell
;; This program is free software
;;; Commentary:
;;; Code:

(require '00_init-hanbetu)

;; @ shell
(require 'shell)
(when run-windows
  (progn
    (setq explicit-shell-file-name "bash.exe")
    (setq shell-command-switch "-c")
    (setq shell-file-name "bash.exe")

    ;; (M-! and M-| and compile.el)
    (setq shell-file-name "bash.exe")
    (modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

    ;; shellモードの時の^M抑制
    (add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

    ;; shell-modeでの補完 (for drive letter)
    (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

    ;; エスケープシーケンス処理の設定
    (autoload 'ansi-color-for-comint-mode-on "ansi-color"
      "Set `ansi-color-for-comint-mode' to t." t)

    (setq shell-mode-hook
          (function
           (lambda ()

             ;; シェルモードの入出力文字コード
             (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
             (set-buffer-file-coding-system    'sjis-unix)
             )))
    )
  )

;;shell-pop***************************
;;@see http://d.hatena.ne.jp/kyagi/20090601
;;http://sakito.jp/emacs/emacsshell.html
;; (req shell-pop)
;; (add-to-list 'shell-pop-internal-mode-list '("multi-term" "*terminal<1>*" '(lambda () (multi-term))))
;; (shell-pop-set-internal-mode "multi-term")


;;shell-toggle************************
;;@see http://gihyo.jp/admin/serial/01/ubuntu-recipe/0038
;; (load-library "~/.emacs.d/elisp/shell-toggle-pathed.el")
;; (autoload 'shell-toggle "shell-toggle"
;;  "Toggles between the *shell* buffer and whatever buffer you are editing."
;;  t)
;; (autoload 'shell-toggle-cd "shell-toggle"
;;  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
;; (global-set-key "\C-ct" 'shell-toggle)
;; (global-set-key "\C-cd" 'shell-toggle-cd)


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
;; (req multi-term)
;; (setq multi-term-program shell-file-name)

;; (add-to-list 'term-unbind-key-list '"M-x")

;; (global-set-key (kbd "C-c t") '(lambda ()
;; 				 (interactive)
;; 				 (multi-term)))

(require 'shellenv)
(shellenv/setpath 'zsh)
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

;; @auto-shell-command
(require 'auto-shell-command)

;; Set of key bindings
(global-set-key (kbd "C-c C-m") 'ascmd:toggle) ; Temporarily on/off auto-shell-command run
(global-set-key (kbd "C-c C-,") 'ascmd:popup)  ; Pop up '*Auto Shell Command*'
(global-set-key (kbd "C-c C-.") 'ascmd:exec)   ; Exec-command specify file name

;; Easier to popup on errors (optional, need '(require 'popwin)')
(push '("*Auto Shell Command*" :height 20) popwin:special-display-config)

;; ;; Notification of results to Growl (optional)
;; (defun ascmd:notify (msg) (deferred:process-shell (format "growlnotify -m %s -t emacs" msg))))

;; ;; Register command
;; (ascmd:add '("Target file's regexp" "Command to be executed when the file save"))
;; ;; A simple example
;; (ascmd:add '("/path/to/dir" "ls"))      ; After you have run the `ls` save the following files: '/path/to/dir'
;; ;; High priority S-exp was evaluated after
;; (ascmd:add '("/path/to/dir/foo.c" "ls -la"))      ; 'foo.c' will only run the `ls-la`
;; ;; Special variables
;; (ascmd:add '("/path/to/dir/.*\.c" "cat $FILE"))      ; Other .c file run the `cat FILE_NAME`(provide '31_init-auto-shell)


(provide '50_init-shell)
;;; 50_init-shell ends here
