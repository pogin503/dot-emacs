;;; 30_init-migemo.el --- 30_init-migemo.el
;;; Commentary:
;; This program is free software
;;; Code:

(when (executable-find "cmigemo")
  (require 'migemo)
  (setq migemo-command "/usr/local/bin/cmigemo")
  ;; -d --dict <dict>       Use a file <dict> for dictionary.
  ;; -s --subdict <dict>    Sub dictionary files. (MAX 8 times)
  ;; -q --quiet             Show no message except results.
  ;; -v --vim               Use vim style regexp.
  ;; -e --emacs             Use emacs style regexp.
  ;; -n --nonewline         Don't use newline match.
  ;; -w --word <word>       Expand a <word> and soon exit.
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init))

(provide '30_init-migemo)
;;; 30_init-migemo.el ends here
