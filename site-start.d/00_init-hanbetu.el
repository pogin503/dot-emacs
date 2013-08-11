;;; 00_init-hanbetu.el --- environment variable
;;; Commentary:
;;; Code:
;; @see https://github.com/murasesyuka/dotemacs
;; @see http://d.hatena.ne.jp/tomoya/20090807/1249601308
;;; OSを判別、UNIX系？:
(defvar run-unix
  (or (equal system-type 'gnu/linux)
      (equal system-type 'usg-unix-v)
      (equal system-type 'berkeley-unix)
      (equal system-type 'cygwin)))

; OSを判別、個別判別
(defvar run-linux
  (equal system-type 'gnu/linux))
(defvar run-system-v
  (equal system-type 'usg-unix-v)); OpenSolaris2090.06
(defvar run-bsd
  (equal system-type 'berkeley-unix))
(defvar run-cygwin ;; cygwinもunixグループにしておく
  (equal system-type 'cygwin))

;; Emacsenの種類とVerを判別
(defvar run-emacs20
  (and (equal emacs-major-version 20)
       (null (featurep 'xemacs))))
(defvar run-emacs21
  (and (equal emacs-major-version 21)
       (null (featurep 'xemacs))))
(defvar run-emacs22
  (and (equal emacs-major-version 22)
       (null (featurep 'xemacs)))); OpenSolaris2090.06
(defvar run-emacs23
  (and (equal emacs-major-version 23)
       (null (featurep 'xemacs))))

;; meadowの種類とVerを判別
(defvar run-meadow (featurep 'meadow))
(defvar run-meadow1 (and run-meadow run-emacs20))
(defvar run-meadow2 (and run-meadow run-emacs21))
(defvar run-meadow3 (and run-meadow run-emacs22))

;; Windowsの判定
(defvar run-nt (equal system-type 'windows-nt))
(defvar run-ms-dos (equal system-type 'ms-dos))
(defvar run-windows
  (or run-nt run-cygwin run-ms-dos run-meadow))

;; OSの64bit判定
(if (and run-windows (file-exists-p "C:/Program Files (x86)"))
    (defvar run-windows-x64 t)
  (defvar run-windows-x64 nil))

;; Macの判定
(defvar run-darwin (equal system-type 'darwin))


(defvar run-xemacs (featurep 'xemacs))
(defvar run-xemacs-no-mule
  (and run-xemacs (not (featurep 'mule))))
(defvar run-carbon-emacs (and run-darwin window-system))

(defvar run-no-window (null window-system))

(provide '00_init-hanbetu)
;;; 00_init-hanbetu.el ends here
