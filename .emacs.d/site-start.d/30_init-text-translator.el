;; text-translator
(require 'text-translator)
  
;;; キーバインド設定
(global-set-key "\C-xt" 'text-translator)
(global-set-key "\C-x\M-T" 'text-translator-translate-last-string)

;; デフォルト翻訳サイトの設定
(setq text-translator-default-engine "excite.co.jp_enja")

;; 自動選択に使用する関数を設定
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)

;; キーバインド設定
(global-set-key "\C-xt" 'text-translator-translate-by-auto-selection)

