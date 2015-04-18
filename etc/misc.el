;;; misc.el --- misc.el -*- lexical-binding: t; coding: utf-8 -*-
;; Author: Ryo
;; Version:
;; Package-Requires: ()
;;; Commentary:
;; This program is free software
;;; Code:

(defun insert-zenkaku-space ()
  "全角空白を挿入."
  (interactive)
  (insert (japanese-zenkaku " ")))

(defun sha1-file (filename)
  "ファイルのチェックサムを出力します."
  (require 'sha1)
  (sha1 (let ((coding-system-for-write 'binary))
          (with-temp-buffer
            (insert-file-contents filename)
            (buffer-string)))))

(fset 'insert-full-width-space #'insert-zenkaku-space)
(fset 'hankaku-region #'japanese-hankaku-region)
(fset 'zenkaku-region #'japanese-zenkaku-region)


;;; misc.el ends here
