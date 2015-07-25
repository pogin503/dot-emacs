;;; 00_init-hanbetu.el --- environment variable
;;; Commentary:
;; @see https://github.com/murasesyuka/dotemacs
;; @see http://d.hatena.ne.jp/tomoya/20090807/1249601308
;;; Code:

;;; OSを判別、UNIX系？:
(defvar run-unix
  (or (equal system-type 'gnu/linux)
      (equal system-type 'usg-unix-v)
      (equal system-type 'berkeley-unix)
      (equal system-type 'cygwin)))

;;; OSを判別、個別判別
(defvar run-linux
  (equal system-type 'gnu/linux))
(defvar run-system-v
  (equal system-type 'usg-unix-v)); OpenSolaris2090.06
(defvar run-bsd
  (equal system-type 'berkeley-unix))
(defvar run-cygwin ;; cygwinもunixグループにしておく
  (equal system-type 'cygwin))

;; Emacsenの種類とVerを判別
(defvar run-emacs22
  (and (equal emacs-major-version 22)
       (null (featurep 'xemacs)))); OpenSolaris2090.06

(defvar run-emacs23
  (and (equal emacs-major-version 23)
       (null (featurep 'xemacs))))

(mapc (lambda (x)
        ;; Emacsのバージョンの判定結果を設定する
        (if (and (equal emacs-major-version 24)
                 (= emacs-minor-version x))
            (set (intern (concat "run-emacs24-" (number-to-string x))) t)
          (set (intern (concat "run-emacs24-" (number-to-string x))) nil)))
      '(1 2 3 4 5))

;; Windowsの判定
(defvar run-nt (equal system-type 'windows-nt))
(defvar run-ms-dos (equal system-type 'ms-dos))
(defvar run-windows
  (or run-nt run-cygwin run-ms-dos))

;; Windowsの64bit判定
(if (and run-windows (file-exists-p "C:/Program Files (x86)"))
    (defvar run-windows-x64 t)
  (defvar run-windows-x64 nil))

;; Macの判定
(defvar run-darwin (or (eq system-type 'darwin)
                       (eq window-system 'ns)))

(provide '00_init-hanbetu)
;;; 00_init-hanbetu.el ends here
