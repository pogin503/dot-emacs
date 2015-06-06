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

;; encoding
(require 'ucs-normalize)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; @see https://gist.github.com/sky-y/3264252
(defun ucs-normalize-NFC-buffer ()
  "バッフ�＞賢��������劫���≪���眼���障��。"
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max))
  )

(global-set-key (kbd "C-x RET u") 'ucs-normalize-NFC-buffer)

(provide '01_init-encoding)
;;; 01_init-encoding ends here
