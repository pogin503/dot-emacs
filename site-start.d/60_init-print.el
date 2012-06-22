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

(require 'cl)
(defun listsubdir (basedir)
  (remove-if (lambda (x) (not (file-directory-p x)))
			 (directory-files basedir t "^[^.]")))

(setq ps-print-color-p t
	  ps-lpr-command "gswin32c.exe"
	  ps-multibyte-buffer 'non-latin-printer
	  ps-lpr-switches '("-sDEVICE=mswinpr2" "-dNOPAUSE" "-dBATCH" "-dWINKANJI")
	  printer-name nil
	  ps-printer-name nil
	  ps-printer-name-option nil
	  ps-print-header nil          ; ヘッダの非表示
      ps-font-size 10
	  )

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
