;;; 90_init-dmacro --- 90_init-dmacro
;; This program is free software
;;; Commentary:
;;; Code:
(defconst *dmacro-key* "\C-t" "繰返し指定キー。")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)
(provide '90_init-dmacro)
;;; 90_init-dmacro ends here
