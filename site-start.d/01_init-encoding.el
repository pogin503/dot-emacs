;;; 01_init-encoding --- 01_init-encoding
;; This program is free software
;;; Commentary:
;; @see http://qiita.com/alpha22jp/items/01e614474e7dbfd78305
;;; Code:
;; 文字コード

(require '00_init-vars)
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


;; https://kokufu.blogspot.com/2016/05/emacs-undecided-unix-cannot-encode.html
(defun my-set-utf-8-in-undecided-encoding ()
  (cond ((string-match "undecided-?.*" (format "%s" buffer-file-coding-system))
         (let ((coding-system-for-read 'utf-8))
           (revert-buffer t t)))))

(add-hook 'find-file-hook
          'my-set-utf-8-in-undecided-encoding)

(add-hook 'server-visit-hook
          (lambda ()
            (if (string-match "COMMIT_EDITMSG" buffer-file-name)
                (set-buffer-file-coding-system 'utf-8))))

(provide '01_init-encoding)
;;; 01_init-encoding ends here
