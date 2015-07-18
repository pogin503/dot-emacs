;;; 31_init-dired --- 31_init-dired
;; This program is free software
;;; Commentary:
;;; Code:

(require 'direx)
(require 'popwin)

(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

(push '(direx:direx-mode :position left :width 30 :dedicated t)
      popwin:special-display-config)

(setq direx:leaf-icon " "
      direx:open-icon "▾ "
      direx:closed-icon "▸ ")

(require '30_mylib-dired)

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "\C-m" 'my-dired-advertised-find-file)
     (define-key dired-mode-map [M-up] 'my-dired-up-directory)
     ;; (define-key dired-mode-map "s" 'my-dired-various-sort-change-or-edit)
     ))

;; dired keybinds memo
;; | C-m    | フォルダを開く時, 新しいバッファを作成しない |
;; | M-<up> | 上のディレクトリに行く |
;; | s      | dired で色々なソートタイプを切り替える |

(require 'wdired)
(setq wdired-allow-to-change-permissions t)
(define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)

;; wdired keybinds memo
;; | e | wdiredモードになる |
;; | C-c C-c | 編集を完了 |
;; | C-c ESC | 編集を中止する |

(provide '31_init-dired)
;;; 31_init-dired ends here
