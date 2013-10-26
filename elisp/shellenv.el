;;; -*- coding: utf-8; lexical-binding: t -*-
;;; shellenv.el --- load environment variables of your shell

;; Author: USAMI Kenta <tadsan@zonu.me>
;; URL: https://github.com/zonuexe/shellenv-el
;; Version: 0.0.5
;; Created: 31 Dec 2012
;; Keywords: internal

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; user customizable variables

(defcustom shellenv/path nil
  "Path to shell which you use."
  :group 'shellenv
  :type  'string)

(defcustom shellenv/shell 'bash
  "Name of your shell"
  :group 'shellenv
  :type  '(choice
           (const :tag "ZShell"   'zsh)
           (const :tag "GNU Bash" 'bash)
           (const :tag "Almquist shell"        'ash)
           (const :tag "Debian Almquist shell" 'dash)
           (const :tag "POSIX.1 shell"         'sh)
           ;;(const :tag "Cmd.exe" 'cmd)
           ;;(const :tag "PowerShell" 'ps1)
           (const :tag "Other"    nil)))

(defcustom shellenv/option nil
  "Arguments of run shell when get a environment variable"
  :group 'shellenv
  :type  'string)

(defcustom shellenv/command nil
  "Command to execute with the shell to get a environment variable"
  :group 'shellenv
  :type  'string)

(defcustom shellenv/env-cmd "/usr/bin/env"
  "Path to `env` Command of your system."
  :group 'shellenv
  :type  'string)

(defcustom shellenv/option-alist
  '(
    ("zsh"  . ("-c" "printenv #{env}"))
    ("bash" . ("-c" "printenv #{env}"))
    ("ash"  . ("-c" "echo $#{env}"))
    ("dash" . ("-c" "echo $#{env}"))
    ("sh"   . ("-c" "echo $#{env}"))
    ; ("ps1" . ("" "Get-Item Env:${env}"))
    ; ("cmd" . ("" "%#{env}%"))
    (nil    . ("-c" "echo $")))
  "Alist of default commandline option of shells"
  :group 'shellenv
  :type 'alist)

;; script local functions
;;; string -> [string]
(defun shellenv/.split-unix-path (paths)
  (split-string paths ":"))

;;; string -> [string]
(defun shellenv/.split-dos-path (paths)
  (split-string paths ";"))

;;; string -> string
;;; (shellenv/.rep-env "PATH" "foo -c #{env}") = > "foo -c PATH"
(defun shellenv/.rep-env (rep string)
  (replace-regexp-in-string "#{env}" rep string))

;;; string -> string
;;; (shellenv/.path2sh "/path/to/zsh") => "zsh"
(defun shellenv/.path2sh (shell-path)
  (let ((l (and shell-path (car (last (split-string shell-path "/"))))))
    (cond
     ((equal shell-path l)  nil)
     ((equal l "")          nil)
     (t                     l))))

;;; string -> string
;;; (shellenv/.envstr "bash") => "/usr/bin/env bash"
(defun shellenv/.envstr (string)
  (concat shellenv/env-cmd " " string))

;;; 'a -> string
;;; (shellenv/.2str 'bash) => "bash"
(defun shellenv/.2str (symbol-or-string)
  (let ((s symbol-or-string))
    (cond
     ((eq s nil)  nil)
     ((symbolp s) (symbol-name s))
     ((stringp s) s)
     (t           nil))))

;;; (str*str*str) -> string
;;; (shellenv/.buildcmd "bash" "-c" "printenv #{env}")
;;;   => "bash -c 'printenv #{env}'"
(defun shellenv/.buildcmd (shell option command)
  (concat shell " " option " '" command "'" ))

;;; string -> string

;;; (shellenv/.firstline "/path/to/foo:/path/to-bar:/path-to/buz
;;; ") => "/path/to/foo:/path/to-bar:/path-to/buz"
(defun shellenv/.firstline (string)
  (let* ((.s (split-string string "\n"))
         (.t (car .s)))
    .t))

;;; () -> symbol
;;; (shellenv/command-string) => "sh-c 'echo ${env}'"
(defun shellenv/command-string (&optional shell)
  (let* ((.pt (shellenv/.path2sh shellenv/path))
         (.st (shellenv/.2str (or shell shellenv/shell .pt)))
         (.opt (car (shellenv/cmdopt .st)))
         (.cmd (cadr (shellenv/cmdopt .st))))
    (shellenv/.buildcmd .st .opt .cmd)))

;;; () -> string
(defun shellenv/cmdopt (shell)
  (or shellenv/command
      (assoc-default
       shell
       shellenv/option-alist)))

;;; (string) -> string
;;; (shellenv/getenv-command-string "PATH") => "sh -c 'echo $PATH'"
(defun shellenv/getenv-command-string (string &optional shell)
  (let* ((.cmd (shellenv/command-string shell)))
    (shellenv/.rep-env string .cmd)))

;;; (string) -> string
(defun shellenv/.getenv (environment-variable-name &optional shell)
  (let* ((.cmd (shellenv/getenv-command-string environment-variable-name shell))
         (.get (shell-command-to-string .cmd))
         (.fst (shellenv/.firstline .get)))
    .fst))

;;; (string) -> (string)
;;; (shellenv/setenv "PATH")
(defun shellenv/setenv (environment-variable-name)
  (let* ((.e (shellenv/.getenv environment-variable-name)))
    (setenv environment-variable-name .e)))

;;; () -> (string)
;;; (shellenv/setpath)
(defun shellenv/setpath (&optional shell)
  (let* ((.p (shellenv/.getenv "PATH" shell))
         (.l (shellenv/.split-unix-path .p)))
    (setenv "PATH" .p)
    (setq-default exec-path (append .l exec-path))
    (setq-default eshell-path-env .p)
    .p))

;;; () -> (string)
(defun shellenv ()
  (shellenv/setpath))

(provide 'shellenv)
