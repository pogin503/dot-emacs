;;; 01_init-encoding --- 01_init-encoding
;; This program is free software
;;; Commentary:
;; @see http://qiita.com/alpha22jp/items/01e614474e7dbfd78305
;;; Code:
;; 文字コード

(set-language-environment "Japanese")

;;改行コード表示
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; encoding
(cond (run-darwin
       (require 'ucs-normalize)
       ;; ファイル名の文字コードを指定
       (setq file-name-coding-system 'utf-8-hfs)
       (setq locale-coding-system 'utf-8-hfs)
       (prefer-coding-system 'utf-8))
      (t
       (setq file-name-coding-system 'utf-8)
       (setq locale-coding-system 'utf-8)))

;; git commit時に文字化けしないために設定する
(setq default-process-coding-system 'utf-8)

;; @see https://gist.github.com/sky-y/3264252
(defun ucs-normalize-NFC-buffer ()
  "バッファ全体の濁点分離を直します."
  (interactive)
  (ucs-normalize-NFC-region (point-min) (point-max)))

(global-set-key (kbd "C-x RET u") 'ucs-normalize-NFC-buffer)

(add-hook 'server-visit-hook
          (lambda ()
            (if (string-match "COMMIT_EDITMSG" buffer-file-name)
                (set-buffer-file-coding-system 'utf-8))))

(provide '01_init-encoding)
;;; 01_init-encoding ends here
