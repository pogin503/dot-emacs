;;; 50_init-magit.el --- 50_init-magit.el
;;; Commentary:
;; @see http://d.hatena.ne.jp/ken_m/20111225/1324833439
;; This program is free software
;;; Code:

(require '00_init-vars)

(use-package magit)

(if run-windows
    (if run-windows-x64
        (progn
          (custom-set-variables
           '(magit-git-executable "C:/Program Files (x86)/Git/bin/git.exe"))
          (add-to-list 'exec-path "C:/Program Files (x86)/Git/bin/git.exe"))
      (progn
        '(magit-git-executable "C:/Program Files/Git/bin/git.exe")
        (add-to-list 'exec-path "C:/Program Files/Git/bin/git.exe"))))

;; (custom-set-variables
;;  ;; '(magit-diff-options '("-w" "-b"))
;;  '(magit-diff-options '()))

;;;
;;; Magit のバッファからファイル名を抽出して利用
;;;

(provide '50_init-magit)
;;; 50_init-magit.el ends here
