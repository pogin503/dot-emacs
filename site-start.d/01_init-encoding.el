;;; 01_init-encoding --- 01_init-encoding
;; This program is free software
;;; Commentary:
;;; Code:
;; 文字コード

(set-language-environment "Japanese")

;;改行コード表示
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; ;; iso-2022-jpをcp50220として扱う。
;; ;; SEMI (cf. http://d.hatena.ne.jp/kiwanami/20091103/1257243524)
;; (add-to-list ‘mime-charset-coding-system-alist
;;              ‘(iso-2022-jp . cp50220))

(provide '01_init-encoding)
;;; 01_init-encoding ends here
