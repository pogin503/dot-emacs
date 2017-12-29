;;; 00_init-vars.el --- 00_init-vars.el -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; This program is free software
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

;; ref from https://github.com/tarsius/no-littering
;; ref https://github.com/Sarcasm/.emacs.d/init.el
(defvar dropbox-directory
  (cond
   ((eq run-windows t) (concat "c:/Users/" user-login-name "/Dropbox/"))
   (t (expand-file-name (convert-standard-filename "Dropbox/") "~/")))
  "Dropbox directory.")

(defun my-expand-dropbox-file-name (file)
  "Expand filename FILE relative to `my-expand-dropbox-file-name'."
  (expand-file-name (convert-standard-filename file)
                    dropbox-directory))

(cl-letf (((symbol-function 'dropbox)
           (symbol-function #'my-expand-dropbox-file-name)))
  (defvar my-emacs-workspace-path (dropbox "100_emacs/"))
  (defvar my-initel-org-path (dropbox "100_emacs/initel.org"))
  (defvar my-org-files-path (dropbox "100_workspace-etc")))

(defvar my-dotfiles-repo-path
  (expand-file-name (convert-standard-filename "dotfiles/") "~/")
  "dotfiles repository path.")

(custom-set-variables
 `(url-history-file ,(locate-user-emacs-file "cache/url/history")))

(defconst my-emacs-repo-dir "~/workspace/emacs/")

(provide '00_init-vars)
;;; 00_init-vars.el ends here
