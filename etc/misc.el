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

(defun my-pp-for-translate ()
  (interactive)
  (save-excursion (replace-regexp "\\([a-zA-Z]\\)
" "\\1 " nil (region-beginning) (region-end))))

(defun my-pp-for-translate-quote ()
  (interactive)
  (save-excursion (replace-regexp "^> " "" nil (region-beginning) (region-end))
                  (replace-regexp "\\([a-zA-Z]\\)
" "\\1 " nil (region-beginning) (region-end))
                  (goto-char (region-beginning))
                  (insert "> ")))

;;; misc.el ends here
