;;; anything-hasktags.el --- hasktags(ctags) anything.el interface

;; Copyright (C) 2011-2012 mori_dev

;; Author: mori_dev <mori.dev.asdf@gmail.com>
;; Keywords: anything, hasktags, haskell
;; Prefix: ah:
;; Version: 0.0.1
;; Compatibility: GNU Emacs 23

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;
;; Features that might be required by this library:
;;
;; `anything'
;;

;;; This file is NOT part of GNU Emacs

;;; Reference
;; Some code referenced from anything-exuberant-ctags.el, anything-etags.el, anything-find-project-resources.el
;;
;; anything-exuberant-ctags.el
;; Author: Kenichirou Oyama <k1lowxb@gmail.com>
;;
;; anything-find-project-resources.el
;; Author: SAKURAI, Masashi <m.sakurai@kiwanami.net>
;;
;; anything-etags.el
;; Author: Kenichirou Oyama <k1lowxb@gmail.com>
;;         Andy Stewart <lazycat.manatee@gmail.com>
;;         rubikitch <rubikitch@ruby-lang.org>
;;         Thierry Volpiatto <thierry.volpiatto@gmail.com>

;;; Commentary:
;;
;; This package use `anything' as a interface to find tag with hasktags(ctags).
;;
;; Follow command is `anything' interface of find hasktags(ctags).
;;
;; `anything-hasktags-select'
;; `anything-hasktags-select-from-here'
;;
;;; Installation:
;;
;; Put anything-hasktags.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'anything-hasktags)
;;
;; It is good to use anything-match-plugin.el to narrow candidates.
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything-match-plugin
;;
;; In your project root directory, do follow command to make tags file.
;;
;; find . -type f -name \*\.*hs | xargs hasktags -c
;;
;; No need more.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-hasktags-select'
;;    Tag jump using `anything'.
;;  `anything-hasktags-select-from-here'
;;    Tag jump with current symbol using `anything'.

;;; Require
(require 'anything)
(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ah:tag-file-name "tags"
  "hasktags(ctags) tag file name.")

(defvar ah:tag-file-search-limit 10
  "The limit level of directory that search tag file.
Don't search tag file deeply if outside this value.")

(defvar ah:line-length-limit 400
  "The limit level of line length.
Don't search line longer if outside this value.")

(defvar ah:line-format-func `ah:line-format
  "The limit level of line length.
Don't search line longer if outside this value.")

(defvar ah:max-length 30
  "Max length for file path name.")

(defvar ah:tag-file-dir nil
  "hasktags(ctags) file directory.")

(defvar ah:recenter-height 5)

(defvar ah:buffer-filename-list nil)

(defvar ah:delete-filename-list nil)

(defvar ah:current-line-overlay
  (make-overlay (point) (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anything-hasktags-select (&optional symbol-name)
  "Tag jump using `anything'.
If SYMBOL-NAME is non-nil, jump tag position with SYMBOL-NAME."
  (interactive)
  (let* ((anything-map ah:anything-map)
         (initial-pattern
          (when symbol-name
              (concat (regexp-quote symbol-name)
                      (when (featurep 'anything-match-plugin) " "))))
         (anything-execute-action-at-once-if-one t))
    (anything '(anything-c-source-hasktags-select)
              ;; Initialize input with current symbol
              initial-pattern "Find Tag: ")))

(defun anything-hasktags-select-from-here ()
  "Tag jump with current symbol using `anything'."
  (interactive)
  (anything-hasktags-select (thing-at-point 'symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar anything-c-source-hasktags-select
  '((name . "hasktags")
    (header-name . anything-source-hasktags-header-name)
    (init . ah:create-buffer)
    (candidates-in-buffer)
    (action ("Goto the location" . ah:goto-location))
    (candidate-number-limit . 999999)
    (persistent-action . ah:persistent-action)
    (cleanup . ah:clean-up)))

(defun ah:get-tag-file ()
  "Get hasktags(ctags) tag file."
  ;; Get tag file from `default-directory' or upper directory.
  (let ((current-dir (ah:find-tag-file default-directory)))
    ;; Return nil if not find tag file.
    (when current-dir
      (setq ah:tag-file-dir current-dir) ;set tag file directory
      (expand-file-name ah:tag-file-name current-dir))))

(defun ah:find-tag-file (current-dir)
  "Find tag file.
Try to find tag file in upper directory if haven't found in CURRENT-DIR."
  (flet ((file-exists? (dir)
           (let ((tag-path (expand-file-name ah:tag-file-name dir)))
             (and (stringp tag-path)
                  (file-exists-p tag-path)
                  (file-readable-p tag-path)))))
    (loop with count = 0
       until (file-exists? current-dir)
       ;; Return nil if outside the value of
       ;; `ah:tag-file-search-limit'.
       if (= count ah:tag-file-search-limit)
       do (return nil)
       ;; Or search upper directories.
       else
       do (incf count)
          (setq current-dir (expand-file-name (concat current-dir "../")))
       finally return current-dir)))

(defun ah:set-buffer-filename-list ()
  (setq ah:buffer-filename-list (ah:get-buffer-filename-list)))

(defun ah:persistent-action (candidate)
     (ah:goto-location candidate)
     (recenter ah:recenter-height)

     ;; todo リファクタ
     (setq tmp-filename
           (file-name-nondirectory (second (split-string candidate "\t"))))

     (unless (member tmp-filename ah:buffer-filename-list)
       (ah:pushnew tmp-filename ah:delete-filename-list))

     (when (overlayp ah:current-line-overlay)
       (move-overlay ah:current-line-overlay
                     (line-beginning-position)
                     (line-end-position)
                     (current-buffer))
       (overlay-put ah:current-line-overlay 'face 'highlight)))

(defun ah:clean-up ()
  (when (overlayp ah:current-line-overlay)
    (delete-overlay ah:current-line-overlay))
    (mapcar 'ah:kill-buffer-if-exist ah:delete-filename-list)
    (setq ah:buffer-filename-list nil)
    (setq ah:delete-filename-list nil))

(defvar ah:anything-map
  (let ((map (copy-keymap anything-map)))
    (define-key map (kbd "C-n")  'ah:next-line)
    (define-key map (kbd "C-p")  'ah:previous-line)
    map))

(defun ah:next-line ()
  (interactive)
  (anything-next-line)
  (ah:anything-execute-persistent-action))

(defun ah:previous-line ()
  (interactive)
  (anything-previous-line)
  (ah:anything-execute-persistent-action))

(defun ah:anything-execute-persistent-action ()
  (unless (zerop (buffer-size (get-buffer (anything-buffer-get))))
    (anything-execute-persistent-action)))

(defun ah:create-buffer ()
  "Create buffer from tag file."
  (anything-aif (ah:get-tag-file)
      (with-current-buffer (anything-candidate-buffer 'global)
        (insert-file-contents it))
    (message "Can't find tag file: %s" it))
  (ah:set-buffer-filename-list))

(defun ah:find-tag (candidate)
  (destructuring-bind (word file line) (split-string candidate "\t")
      (setq line (string-to-int line))
      (find-file file)
      (goto-line line)))

(defun ah:goto-location (candidate)
  (ah:find-tag candidate)
  (when (and anything-in-persistent-action
             (fboundp 'anything-match-line-color-current-line))
    (anything-match-line-color-current-line)))

(defun anything-source-hasktags-header-name (x)
  (concat "hasktags(ctags) in "
          (with-current-buffer anything-current-buffer
            (ah:get-tag-file))))

(defun ah:get-buffer-filename-list ()
  (mapcar 'file-name-nondirectory
          (remove-if 'null
                     (mapcar 'buffer-file-name (buffer-list)))))

(defun ah:kill-buffer-if-exist (buf)
  (when (get-buffer buf)
      (kill-buffer buf)))

(defmacro ah:pushnew (x place &rest keys)
  (if (symbolp place)
      (if (null keys)
          `(let ((x ,x))
             (if (member x ,place) ,place (setq ,place (cons x ,place))))
        (list 'setq place (list* 'adjoin x place keys)))
    (list* 'callf2 'adjoin x place keys)))

(provide 'anything-hasktags)
;;; anything-hasktags.el ends here
