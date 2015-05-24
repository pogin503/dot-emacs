;;; 30_init-quickrun --- 30_init-quickrun
;; This program is free software
;;; Commentary:
;;; Code:
(require 'quickrun)

;; 結果の出力バッファと元のバッファを行き来したい場合は
;; ':stick t'の設定をするとよい
(push '("*quickrun*") popwin:special-display-config)

;; よく使うならキーを割り当てるとよいでしょう
(global-set-key (kbd "<f5>") 'quickrun)
(global-set-key (kbd "<f6>") 'quickrun-compile-only)
(provide '30_init-quickrun)
;;; 30_init-quickrun ends here
