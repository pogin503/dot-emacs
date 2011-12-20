;;;; 30_init-smartchr.el ---- smartchr
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) pogin

;; Author: pogin
;; Keywords: tools

;;; License
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the

;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; @see https://github.com/handle/emacs-settings
;; @see https://bitbucket.org/sakito/dot.emacs.d/src/7a6d89a5b3c6/site-start.d/init_smartchr.el
;; @see http://d.hatena.ne.jp/tequilasunset/20101119/p1
;;
;; INSTALL
;; (install-elisp "https://github.com/imakado/emacs-smartchr/raw/master/smartchr.el")

(req smartchr)
(defun my-smartchr-braces ()
  "Insert a pair of braces like below.
\n    {\n    `!!'\n}"
  ;; foo {
  ;;     `!!'
  ;; }
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert "{\n\n}")
                  (indent-region beg (point))
                  (forward-line -1)
                  (indent-according-to-mode)
                  (goto-char (point-at-eol))
                  (setq end (save-excursion
                              (re-search-forward "[[:space:][:cntrl:]]+}" nil t))))
     :cleanup-fn (lambda ()
                   (delete-region beg end))
     )))

(defun my-smartchr-comment ()
  "Insert a multiline comment like below.
\n/*\n * `!!'\n */"
  ;; /*
  ;;  * `!!'
  ;;  */
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert "/*\n* \n*/")
                  (indent-region beg (point))
                  (setq end (point))
                  (forward-line -1)
                  (goto-char (point-at-eol)))
     :cleanup-fn (lambda ()
                   (delete-region beg end))
     )))
(defun my-smartchr-comment-doxygen ()
  "Insert a multiline comment like below.
\n/*\n * `!!'\n */"
  ;; /*
  ;;  * `!!'
  ;;  */
  (lexical-let (beg end)
    (smartchr-make-struct
     :insert-fn (lambda ()
                  (setq beg (point))
                  (insert "/**\n* \n*/")
                  (indent-region beg (point))
                  (setq end (point))
                  (forward-line -1)
                  (goto-char (point-at-eol)))
     :cleanup-fn (lambda ()
                   (delete-region beg end))
     )))

(defun my-smartchr-semicolon ()
  "Insert a semicolon at end of line."
  (smartchr-make-struct
   :insert-fn (lambda ()
                (save-excursion
                  (goto-char (point-at-eol))
                  (insert ";")))
   :cleanup-fn (lambda ()
                 (save-excursion
                   (goto-char (point-at-eol))
                   (delete-backward-char 1)))
   ))

(defun my-smartchr-keybindings ()
  ;; !! がカーソルの位置
  (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
  (local-set-key (kbd "+") (smartchr '("+" " + " "++" " += ")))
  (local-set-key (kbd "-") (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "[")))
  (local-set-key (kbd "{") (smartchr '(my-smartchr-braces "{" "{`!!'}")))
  ;;バッククォート
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  ;;ダブルクォーテーション
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  ;;シングルクォート
  (local-set-key (kbd "\'") (smartchr '("\'`!!'\'" "\'")))
  (local-set-key (kbd ">") (smartchr '(">" " > " " => " " >> ")))
  (local-set-key (kbd "<") (smartchr '("<" " < " " << " "<`!!'>")))
  (local-set-key (kbd ",") (smartchr '(", " ",")))
  (local-set-key (kbd ".") (smartchr '("." " . ")))
  (local-set-key (kbd "?") (smartchr '("?" "? `!!' " "<?`!!'?>")))
  (local-set-key (kbd "!") (smartchr '("!" " != ")))
  (local-set-key (kbd "&") (smartchr '("&" " && ")))
  (local-set-key (kbd "|") (smartchr '("|" " || ")))
  (local-set-key (kbd "/") (smartchr '("/" "/* `!!' */" my-smartchr-comment)))
  (local-set-key (kbd ";") (smartchr '(";" my-smartchr-semicolon)))
  )
(defun my-smartchr-keybindings-cpp ()
  ;; !! がカーソルの位置
  (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
  (local-set-key (kbd "+") (smartchr '("+" " + " "++" " += ")))
  (local-set-key (kbd "-") (smartchr '("-" " - " "--" " -= ")))
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "[")))
  (local-set-key (kbd "{") (smartchr '(my-smartchr-braces "{" "{`!!'}")))
  ;;バッククォート
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  ;;ダブルクォーテーション
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  ;;シングルクォート
  (local-set-key (kbd "\'") (smartchr '("\'`!!'\'" "\'")))
  (local-set-key (kbd ">") (smartchr '(">" " > " " => " " >> ")))
  (local-set-key (kbd "<") (smartchr '("<" " < " " << " "<`!!'>")))
  (local-set-key (kbd ",") (smartchr '(", " ",")))
  (local-set-key (kbd ".") (smartchr '("." " . ")))
  (local-set-key (kbd "?") (smartchr '("?" "? `!!' " "<?`!!'?>")))
  (local-set-key (kbd "!") (smartchr '("!" " != ")))
  (local-set-key (kbd "&") (smartchr '("&" " && ")))
  (local-set-key (kbd "|") (smartchr '("|" " || ")))
  (local-set-key (kbd "/") (smartchr '("/" "/** `!!' */" my-smartchr-comment-doxygen)))
  (local-set-key (kbd ";") (smartchr '(";" my-smartchr-semicolon)))
  )

(defun my-smartchr-keybindings-lisp ()
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "[")))
  (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd ".") (smartchr '("." " . ")))
  )

(defun my-smartchr-keybindings-objc ()
  (local-set-key (kbd "@") (smartchr '("@\"`!!'\"" "@"))))

(dolist (hook (list
               'c-mode-common-hook
               'php-mode-hook
               'ruby-mode-hook
               'cperl-mode-hook
               'javascript-mode-hook
               'js-mode-hook
               'js2-mode-hook
               'text-mode-hook
               ))
  (add-hook hook 'my-smartchr-keybindings))

(add-hook 'c++-mode-hook 'my-smartchr-keybindings-cpp)

(dolist (hook (list
               'lisp-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'inferior-gauche-mode-hook
               'scheme-mode-hook
               ))
  (add-hook hook 'my-smartchr-keybindings-lisp))


(add-hook 'objc-mode-hook 'my-smartchr-keybindings-objc)

(defun my-smartchr-keybindings-html ()
  ;; !! がカーソルの位置
  (local-set-key (kbd "=") (smartchr '("=" " = " " == ")))
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[[`!!']]" "[")))
  (local-set-key (kbd "{") (smartchr '("{" "{`!!'}" my-smartchr-braces)))
  ;;ダブルクォート
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  ;;シングルクォート
  (local-set-key (kbd "\'") (smartchr '("\'`!!'\'" "\'")))
  (local-set-key (kbd "<") (smartchr '("<`!!'>" "</`!!'>" "<")))
  (local-set-key (kbd ",") (smartchr '(", " ",")))
  (local-set-key (kbd ".") (smartchr '("." " . ")))
  (local-set-key (kbd "?") (smartchr '("?" "? `!!' " "<?`!!'?>")))
  (local-set-key (kbd "!") (smartchr '("!" "<!-- `!!' -->")))
  (local-set-key (kbd ";") (smartchr '(";" my-smartchr-semicolon)))
  (local-set-key (kbd ":") (smartchr '(": " ":")))
  (local-set-key (kbd "/") (smartchr '("/" "/* `!!' */" my-smartchr-comment)))
  )
(dolist (hook (list
               'html-mode-hook
               'css-mode-hook
               ))
  (add-hook hook 'my-smartchr-keybindings-html))

;;; 30_init-smartchr.el ends here
