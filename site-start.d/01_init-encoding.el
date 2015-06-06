;;; 01_init-encoding --- 01_init-encoding
;; This program is free software
;;; Commentary:
;;; Code:
;; æ–‡å­—ã‚³ãƒ¼ãƒ‰

(set-language-environment "Japanese")

;;æ”¹è¡Œã‚³ãƒ¼ãƒ‰è¡¨ç¤º
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; ;; iso-2022-jpã‚’cp50220ã¨ã—ã¦æ‰±ã†ã€‚
;; ;; SEMI (cf. http://d.hatena.ne.jp/kiwanami/20091103/1257243524)
;; (add-to-list â€˜mime-charset-coding-system-alist
;;              â€˜(iso-2022-jp . cp50220))

;; encoding
(require 'ucs-normalize)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8-hfs)
(setq locale-coding-system 'utf-8-hfs)

;; @see https://gist.github.com/sky-y/3264252
(defun ucs-normalize-NFC-buffer ()
  "ãƒãƒƒãƒ•ã‚ï¼è³¢ãõ€”‹¿ç‚åŠ«ˆ†é›â‰ª‚’ç›çœ¼—ãéšœ™ã€‚"
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max))
  )

(global-set-key (kbd "C-x RET u") 'ucs-normalize-NFC-buffer)

(provide '01_init-encoding)
;;; 01_init-encoding ends here
