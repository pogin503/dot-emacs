(require 'quickrun)

;; 結果の出力バッファと元のバッファを行き来したい場合は
;; ':stick t'の設定をするとよいでしょう
(push '("*quickrun*") popwin:special-display-config)

;; よく使うならキーを割り当てるとよいでしょう
(global-set-key (kbd "<f5>") 'quickrun)
