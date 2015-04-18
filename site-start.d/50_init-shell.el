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

;; ansi-colorでエスケープシーケンスをfontifyする設定
;;@see http://d.hatena.ne.jp/rubikitch/20081102/1225601754
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Set of key bindings
(global-set-key (kbd "C-c C-m") 'ascmd:toggle) ; Temporarily on/off auto-shell-command run
(global-set-key (kbd "C-c C-,") 'ascmd:popup)  ; Pop up '*Auto Shell Command*'
(global-set-key (kbd "C-c C-.") 'ascmd:exec)   ; Exec-command specify file name

(provide '50_init-shell)
;;; 50_init-shell ends here
