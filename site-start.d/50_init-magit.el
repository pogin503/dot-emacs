;;; 50_init-magit.el --- 50_init-magit.el
;;; Commentary:
;; @see http://d.hatena.ne.jp/ken_m/20111225/1324833439
;; This program is free software
;;; Code:

(require '00_init-hanbetu)

(autoload 'magit-status "magit" nil t)
(if run-windows
    (if run-windows-x64
        (progn
          (setq magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe")
          (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin/git.exe"))
      (progn
        (setq magit-git-executable "C:/Program Files/Git/bin/git.exe")
        (add-to-list 'exec-path "C:/Program Files/Git/bin/git.exe"))))

;; (setq magit-diff-options '("-w" "-b"))
(setq magit-diff-options '())

;;;
;;; Magit のバッファからファイル名を抽出して利用
;;;

(add-hook 'magit-mode-hook
          ;; Magit で有効なキー設定
          (lambda ()
            (local-set-key (kbd "@") 'my-magit-select-files)
            (local-set-key (kbd "`") 'my-magit-clear-selected-files)))

(provide '50_init-magit)
;;; 50_init-magit.el ends here
