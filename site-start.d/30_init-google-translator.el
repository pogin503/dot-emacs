;;; 30_init-google-translator.el --- 30_init-google-translator.el
;; This program is free software
;;; Commentary:
;;; Code:

(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-ct" 'google-translate-at-point)
(global-set-key "\C-cT" 'google-translate-query-translate)

;; | C-c t | ポイント下の単語を翻訳 |
;; | C-c T | ミニバッファから読み込む |

(provide '30_init-google-translator)
;;; 30_init-google-translator.el ends here
