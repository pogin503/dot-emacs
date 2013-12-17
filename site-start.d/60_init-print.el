;;; 60_init-print --- 60_init-print
;; This program is free software
;;; Commentary:
;;; Code:
;; ; ghostscriptの実行コマンド場所を指定
;; (setq ps-print-color-p t
;;       ps-lpr-command "c:/gs/gs8.71/bin/gswin32c.exe"
;;       ps-multibyte-buffer 'non-latin-printer
;;       ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
;;       printer-name nil
;;       ps-printer-name nil
;;       ps-printer-name-option nil
;;       ps-print-header nil          ; ヘッダの非表示
;;       )

(eval-when-compile
  (require 'cl))

;;;====================================
;;;; print - 印刷設定
;;;====================================
;;; Postscript で印刷
(setq my-print-command-format "nkf -e | e2ps -a4 -p | lpr")
(defun my-print-region (begin end)
  (interactive "r")
  (shell-command-on-region begin end my-print-command-format))
(defun my-print-buffer ()
  (interactive)
  (my-print-region (point-min) (point-max)))

(defun listsubdir (basedir)
  (remove-if (lambda (x) (not (file-directory-p x)))
			 (directory-files basedir t "^[^.]")))
(require '00_init-hanbetu)

(if run-windows
    (setq ps-print-color-p t
          ps-lpr-command "gswin32c.exe"
          ps-multibyte-buffer 'non-latin-printer
          ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
          printer-name nil
          ps-printer-name nil
          ps-printer-name-option nil
          ps-print-header nil          ; ヘッダの非表示
          ps-font-size 10
          ))

;; 印刷する場合はps-print-bufferとかを使う

;; (require 'ps-mule)

;; (setq ps-paper-type 'a4      ;; 用紙タイプ
;;       lpr-command "lpr"      ;;
;;       ps-lpr-command "lpr"
;;       ps-multibyte-buffer 'non-latin-printer
;;       ps-n-up-printing 1
;;       ps-left-margin 20
;;       ps-right-margin 20
;;       ps-top-margin 20
;;       ps-bottom-margin 20
;;       ps-n-up-margin 20
;;       ;;(setq ps-font-size '(9 . 10))
;;       ps-font-size 7)
(provide '60_init-print)
;;; 60_init-print ends here
