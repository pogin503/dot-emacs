;;; key-combo.el --- map key sequence to commands

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2011, 2012 Yuuki Arisawa
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;; Author: Yuuki Arisawa <yuuki.ari@gmail.com>
;; URL: https://github.com/uk-ar/key-combo
;; Created: 30 November 2011
;; Version: 1.5.1
;; Keywords: keyboard input

;;; Commentary:

;; ########   Compatibility   ########################################
;;
;; Works with Emacs-23.2.1, 23.1.1

;; ########   Quick start   ########################################
;;
;; Add to your ~/.emacs
;;
;; (require 'key-combo)
;; (key-combo-mode 1)
;;
;; and some chords, for example
;;
;;  (key-combo-define-global (kbd "=") '(" = " " == " " === " ))
;;  (key-combo-define-global (kbd "=>") " => ")
;;
;; or load default settings
;;
;;  (key-combo-load-default)

;;; History:

;; Revision 1.5.1 2012/06/06 21:36:28
;; * Bug fix which use flex-autopair by mistake.
;;
;; Revision 1.5 2012/04/20 22:24:26
;; * Bug fix when just after string.
;; * Add !== for js and php's not triple-equal by tomykaira.
;; * Change some default settings.
;;
;; Revision 1.4.1 2012/04/04 21:05:48
;; * Bug fix for first key in c-mode and other modes.
;;
;; Revision 1.4 2012/04/03 20:15:21
;; * Regard first key as key-combo-execute-original when first key is not assigned
;; * Auto indent when inserting string have new line
;;
;; Revision 1.3 2012/03/13 22:00:23
;; * Make works well for other elisp which use post command hook
;;
;; Revision 1.2 2012/02/10 22:15:52
;; * Add support to use SKK. Bug reported by ballforest
;; * Bug fix for html mode.
;;
;; Revision 1.1 2012/02/08 21:56:27
;; * Add key-combo-define-local function to set key for local keymap.
;; * Add a lot of default setting in pogin's blog.
;;
;; Revision 1.0 2012/01/31 22:03:50
;; * Change clean-up function to use undo
;;
;; Revision 0.7 2012/01/17 21:25:10
;; * Insert white space dwim
;;
;; Revision 0.6 2012/01/16 21:17:01
;; * Allow cleanup function as nil
;; * Add key-combo-return function,
;; which can move to point of command beginning.
;; * Allow meta key for key-combo key.
;; * Save undo history when self-insert-command.
;;
;; Revision 0.5 2012/01/13 23:02:39
;; * Support function as key-combo command
;;
;; Revision 0.4
;; * Map key to minor mode to toggle enable and disable.
;;
;; Revision 0.3
;; * Not to cleanup when 1 sequence key
;; * Bugfix by tomykaira
;; * Refactoring
;; * Add test cases
;;
;; Revision 0.2
;; * First release
;;
;; Revision 0.1
;; * Initial revision

;; Code goes here
(require 'cl)
;; for remove-if
(defvar key-combo-debug nil)

(defun key-combo-describe ()
  "List key combo bindings in a help buffer."
  (interactive)
  (describe-bindings [key-combo]))

;; (mac-input-source-is-ascii-capable)

(defun key-combo-make-key-vector (key)
  "Return vector
key is sequences"
  (vector 'key-combo
          ;; "_" is for error when key is " "
          (intern (concat "_" (key-description (vconcat key))))))

(defun key-combo-key-binding (key)
  ;; copy from `key-binding'
  "Return the binding for command KEY in key-combo keymaps.
KEY is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition."
  (key-binding (key-combo-make-key-vector (vconcat key))))

(defun key-combo-lookup-key (keymap key)
  ;; copy from `key-binding'
  "Return the binding for command KEY in key-combo keymaps.
KEY is a string or vector, a sequence of keystrokes.
The binding is probably a symbol with a function definition."
    (lookup-key keymap (key-combo-make-key-vector (vconcat key))))

;; should be replace by union?
(defun key-combo-memq (a b)
  (setq a (if (consp a) a (list a)))
  (setq b (if (consp b) b (list b)))
  (apply
   'append
   (delete-if
    'null
    (mapcar
     (lambda (x) (if (memq x b) (list x) nil))
     a))))

;; From context-skk.el
;; http://openlab.ring.gr.jp/skk/skk/main/context-skk.el
(defun key-combo-comment-or-stringp ()
  (if (or (nth 4 (syntax-ppss));; comment
          (nth 3 (syntax-ppss));; string
          )
      t
    nil))

(defun key-combo-execute-macro (string)
  (cond
   ((string-match "`!!'" string)
    (destructuring-bind (pre post) (split-string string "`!!'")
      (key-combo-execute-macro pre)
      (save-excursion
        (key-combo-execute-macro post))
      ))
   (t
    (let ((p (point)))
      (if (and (eq ?  (char-before))
               (eq ?  (aref string 0)))
          (delete-char -1))
      (insert string)
      (when (string-match "\n" string)
        (indent-according-to-mode)
        (indent-region p (point)))))))

(defun key-combo-get-command (command)
  (unless (key-combo-elementp command)
    (error "%s is not command" command))
  (cond
   ((functionp command) command)
   ((listp command) command)
   ((stringp command)
    ;; do not map string because it changes this-command-keys in post-command-hook
    (lexical-let ((com command))
      (lambda ()
        (interactive)
        (when (and (not (null buffer-undo-list))
                   (not (eq buffer-undo-list t))
                   (eq (car buffer-undo-list) nil))
          (setq buffer-undo-list (cdr buffer-undo-list)))
        (key-combo-execute-macro com))))
   ((vectorp command)
    (lexical-let ((com command))
      (lambda ()
        (interactive)
        (key-combo-mode -1)
        (execute-kbd-macro com)
        (key-combo-mode 1)
        )
      ))
   (t (error "%s is not command" command))
   );;end cond
  )

(defun key-combo-elementp (element)
  (or ;;(functionp element)
   (commandp element)
      (stringp element)
      (null element));;for unset key
  )

(defun key-combo-define (keymap key commands)
  "In KEYMAP, define key sequence KEY as COMMANDS.
KEYMAP is a keymap.\n
KEY is a string or a vector of symbols and characters meaning a
sequence of keystrokes and events.  Non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.\n
COMMANDS can be an interactive function, a string, nil, or list of these COMMAND.
If COMMANDS is string, treated as a smartchr flavor keyboard macro.
If COMMANDS is nil, the key-chord is removed.
If COMMANDS is list, treated as sequential commands.
"
  ;;copy from key-chord-define
  (let ((base-key (list (car (listify-key-sequence key))));; list
        (last-key (last (listify-key-sequence key))));; list
    (cond
     ;;for sequence '(" = " " == ")
     ((and (not (key-combo-elementp commands))
           (car-safe commands)
           (key-combo-elementp (car-safe commands)))
      (let ((seq-keys base-key));;list
        (mapc #'(lambda(command)
                  (key-combo-define keymap (vconcat seq-keys) command)
                  (setq seq-keys
                        (append seq-keys base-key)))
              commands)))
     (t
      (unless (key-combo-elementp commands)
        (error "%s is not command" commands))
      ;; regard first key as key-combo-execute-original
      (let ((first (lookup-key keymap
                               (key-combo-make-key-vector base-key))))
        (when
            (and (eq (safe-length (listify-key-sequence key)) 2)
                 (null first))
          (key-combo-define keymap
                            (vconcat base-key)
                            'key-combo-execute-original)))
      (when (keymapp (lookup-key keymap (vconcat last-key)))
        ;; this is for prefix command
        ;; (key-combo-define-prefix (vconcat last-key)
        ;;                          'key-combo-prefix-command)
        )
      (when key-combo-debug
        (message
         "%s |%s"
         (mapconcat
          (lambda (k)
            (if (null k) " " (single-key-description k)))
          ;; nthcar
          (reverse (last (reverse (append key (make-list 3 nil) nil)) 3))
          "|")
         (cond ((stringp commands) (concat "`" commands "`"))
               ((vectorp commands) (concat "<kbd>"
                                           (key-description commands)
                                           "</kbd>"))
               ;; ((eq commands 'key-combo-execute-original))
               (t (format "%S" commands))
               )
         ))
      (define-key keymap
        (key-combo-make-key-vector key)
        (key-combo-get-command commands))
      ))))

(defun key-combo-define-global (keys command)
  "Give KEY a global binding as COMMAND.\n
See also `key-combo-define'\n
Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function.
"
  ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-combo-define (current-global-map) keys command))

(defun key-combo-define-local (keys command)
  "Give KEY a local binding as COMMAND.\n
See also `key-combo-define'\n
The binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode.
"
  ;;(interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-combo-define (current-local-map) keys command))

(defvar key-combo-start-position nil)

(defun key-combo-set-start-position (pos)
  (setq key-combo-start-position pos))

(defun key-combo-return ()
  "Return to the position when sequence of calls of the same command was started."
  (interactive)
  (unless (eq key-combo-start-position nil)
    (progn
      (goto-char (car key-combo-start-position))
      ;; (set-window-start (selected-window) (cdr key-combo-start-position))
      )))

;;(browse-url "http://q.hatena.ne.jp/1226571494")
(defun key-combo-count-boundary (last-undo-list)
  (length (remove-if-not 'null last-undo-list)))

(defun key-combo-undo ()
  "returns buffer undo list"
  (primitive-undo (1+ (key-combo-count-boundary buffer-undo-list))
                  buffer-undo-list)
  )

;;;###autoload
(define-minor-mode key-combo-mode
  "Toggle key combo."
  :lighter " KC"
  :group 'key-combo
  :keymap (make-sparse-keymap)
  (if key-combo-mode
      (add-hook 'post-command-hook #'key-combo-post-command-function t t)
    (remove-hook 'post-command-hook #'key-combo-post-command-function t)
      ;; (add-hook 'pre-command-hook
      ;;           ;;post-self-insert-hook
      ;;           #'key-combo-pre-command-function nil t)
      ;; (remove-hook 'pre-command-hook
      ;;            #'key-combo-pre-command-function t)
    )
  )

(defcustom key-combo-disable-modes nil
  "Major modes `key-combo-mode' can not run on."
  :group 'key-combo)

;; copy from auto-complete-mode-maybe
(defun key-combo-mode-maybe ()
  "What buffer `key-combo-mode' prefers."
  (when (and (not (minibufferp (current-buffer)))
             (not (memq major-mode key-combo-disable-modes))
             (key-combo-mode 1)
             ;; (key-combo-setup)
             )))

;; copy from global-auto-complete-mode
;;;###autoload
(define-global-minor-mode global-key-combo-mode
  key-combo-mode
  key-combo-mode-maybe ;turn on
  ;; :init-value t bug?
  :group 'key-combo)

;; new key combo
;; can not use recent-keys because it does't record keyborad macro
;; (defvar key-combo-count 0)
(defun key-combo-keys-vector ()
  (vconcat (read-kbd-macro
            (substring (symbol-name
                        (when (symbolp last-nonmenu-event) last-nonmenu-event)
                        ) 1))))

(defun key-combo-unread-events (vector)
  ;;cannot use push because need to concat vector and list
  (setq unread-command-events
        (append vector
                unread-command-events))
  ;;(reset-this-command-lengths)
  )

(defun key-combo-execute-original ()
  (interactive)
  ;; for self-insert-command
  (setq last-command-event (aref (key-combo-keys-vector) 0))
  (call-interactively (key-binding (vector last-command-event)))
  )

;; this is for debug
(defadvice key-combo-post-command-function (around intercept activate)
  (condition-case err
      ad-do-it
    ;; Let the debugger run
    ((debug error) (signal (car err) (cdr err)))))

;;input-method-function

(defun key-combo-post-command-function ()
  (let* ((echo-keystrokes 0)
         (this-command nil)
         (in-key-combo (eq 'key-combo
                           (if (< 0 (length (this-command-keys-vector)))
                               (aref (this-command-keys-vector) 0))))
         (keys-vector (if in-key-combo (key-combo-keys-vector) nil))
         (events (vector (read-event)));; read-key
         )
    (cond
     ;; finish:disabled modes
     ((or (not key-combo-mode)
          (minibufferp)
          isearch-mode
          (and (key-combo-comment-or-stringp)
               (not in-key-combo)))
      (when (and in-key-combo (not (eq t buffer-undo-list)))
        (setq buffer-undo-list
              (append buffer-undo-list key-combo-original-undo-list)))
      (key-combo-unread-events events)
      )
     ;; keep
     ((and in-key-combo
           (key-combo-key-binding (vconcat keys-vector events)))
      (undo-boundary)
      (primitive-undo
       (+ 1 (key-combo-count-boundary buffer-undo-list)) buffer-undo-list)
      (key-combo-unread-events
       (key-combo-make-key-vector (vconcat keys-vector events)))
      ;; http://www.update.uu.se/~ams/public_html/emacs/test/automated/undo-tests.el
      )
     ;; loop
     ((and in-key-combo
           (not (key-combo-key-binding (vconcat keys-vector events)))
           ;; for 1 key eg.<key-combo> SPC SPC
           (not (eq (length (vconcat keys-vector events)) 2))
           (key-combo-key-binding events)
           ;; all same key
           (equal [] (delete (aref (vconcat keys-vector events) 0)
                             (vconcat keys-vector events))))
      ;; finish
      (undo-boundary)
      (primitive-undo
       (+ 1 (key-combo-count-boundary buffer-undo-list)) buffer-undo-list)
      (key-combo-unread-events (key-combo-make-key-vector events))
      )
     ;; fall back prefix key
     ((and in-key-combo
           (not (key-combo-key-binding (vconcat keys-vector events)))
           (key-binding (vconcat keys-vector events)))
      ;; finish
      ;; fall back prefix key
      ;; Todo: multiple prefix key
      (undo-boundary)
      (primitive-undo
       (+ 1 (key-combo-count-boundary buffer-undo-list)) buffer-undo-list)
      (key-combo-unread-events (vconcat keys-vector events))
      )
     ;; start
     ((key-combo-key-binding events)
      (setq key-combo-original-undo-list buffer-undo-list
            buffer-undo-list nil)
      (key-combo-set-start-position (cons (point) (window-start)))
      ;;enables cancel for insertion
      (when (memq (key-binding events)
                  '(self-insert-command skk-insert))
        ;; (eq (key-binding events) 'self-insert-command)
        (undo-boundary)
        (setq last-command-event (aref events 0))
        (call-interactively (key-binding events))
        (undo-boundary)
        (primitive-undo
         (+ 1 (key-combo-count-boundary buffer-undo-list)) buffer-undo-list)
        )
      (key-combo-unread-events (key-combo-make-key-vector events))
      )
     ;; finish
     (t
      (when (and in-key-combo (not (eq t buffer-undo-list)))
        (setq buffer-undo-list
              (append buffer-undo-list key-combo-original-undo-list)))
      (key-combo-unread-events events)
      ;; no key combo
      )
     )
    (unless prefix-arg
      (reset-this-command-lengths)
      )
    )
  )

;;; default configuration

;; < { [ should use flex-autopair
(defvar key-combo-global-default
  '(;; instead of using (goto-char (point-min))
    ;; use beginning-of-buffer for keydescription
    ("C-a"   . (back-to-indentation move-beginning-of-line
                                    beginning-of-buffer key-combo-return))
    ("C-e"   . (end-of-line end-of-buffer key-combo-return))
    ))

(defvar key-combo-lisp-default
  `(("."  . (key-combo-execute-original))
    (". SPC" . " . ")
    ("SPC"  . (key-combo-execute-original))
    ("SPC ." . " . ")
    (","  . (key-combo-execute-original))
    (",@" . " ,@");; for macro
    ;; (";"  . ";; ")
    (";"  . (";; " ";;; " "; ")) ;cannot use because of comment
    (";=" . ";=> ");; for xmp
    ("="  . ("= " "eq " "equal "))
    (">=" . ">= ")
    ("C-M-x" . (key-combo-execute-original ,(kbd "C-u C-M-x")));; lamda for message
    ;; ("-"  . (key-combo-execute-original));; for symbol name
    ;; ("/" . ("/`!!'/" "/* `!!' */") );;for regexp, comment
    ))

(defvar key-combo-lisp-mode-hooks
  '(lisp-mode-hook
    emacs-lisp-mode-hook
    lisp-interaction-mode-hook
    inferior-gauche-mode-hook
    scheme-mode-hook))

(defun key-combo-read-kbd-macro (start)
  (when (or (equal (elt start 0) ?\ )
            (equal (elt start (1- (length start))) ?\ ))
    ;; (error "To bind the key SPC, use \" \", not [SPC]")
    (error "To bind the key SPC, use SPC, not \" \""))
  (read-kbd-macro start))

(defmacro define-key-combo-load (name)
  "define-key-combo-load is deprecated"
  `(defun ,(intern (concat "key-combo-load-" name "-default")) ()
     (dolist (key ,(intern (concat "key-combo-" name "-default")))
       (key-combo-define-local (key-combo-read-kbd-macro (car key)) (cdr key)))
     ))

;; for algol like language
(defcustom key-combo-common-mode-hooks
  '(c-mode-common-hook;; It's run immediately before the language specific hook.
    php-mode-hook
    ruby-mode-hook
    cperl-mode-hook
    perl-mode-hook
    python-mode-hook
    javascript-mode-hook
    js-mode-hook
    js2-mode-hook
    )
  "Hooks that enable `key-combo-common-default' setting"
  :group 'key-combo)

;; (browse-url "http://bojovs.github.com/2012/04/24/ruby-coding-style/")
(defcustom key-combo-common-default
  '((","  . ", ")
    ("="  . (" = " " == " " === " ));;" === " for js
    ("=>" . " => ")
    ("=~" . " =~ ");;for ruby regexp
    ("=*" . " =* ")                     ;for c
    ("+"  . (" + " "++"))
    ("+=" . " += ")
    ;; ("-"  . (" - " "--"))                ;undo when unary operator
    ("-"  . ("-" "--"))               ;abuse in regexp
    ("-=" . " -= ")
    ("->" . " -> ");; for haskell,coffee script. overwrite in c
    (">"  . (key-combo-execute-original " >> "))
    ;; " > " should be bind in flex-autopair
    (">=" . " >= ")
    (">>=" . " >>= ")
    ("%"  . " % ")
    ("%="  . " %= ")
    ("^"  . " ^ ");; XOR for c
    ("^="  . " ^= ");; for c
    ("!" . key-combo-execute-original)
    ;; NOT for c
    ;; don't use " !" because of ruby symbol
    ;; and unary operator
    ("!="  . " != " ) ;;" !== " for js and php
    ("!==" . " !== ") ;;" !== " for js and php
    ("!~" . " !~ ")   ; for ruby
    ("~" . key-combo-execute-original)
    ;; for unary operator
    ;; ("::" . " :: ") ;; for haskell not in ruby
    ;; (":" . ":");;for ruby symbol
    ("&"  . (" & " " && "))             ;overwrite in c
    ("&=" . " &= ");; for c
    ("&&=" . " &&= ")                   ; for ruby
    ("*"  . " * " )                     ;overwrite in c
    ("*="  . " *= " )
    ("**"  . "**" )                     ;for power
    ("**=" . " **=" )                     ;for power
    ;; ("?" . "? `!!' :"); ternary operator should be bound in yasnippet?
    ;; ("?=");; for coffeescript?
    ("<" . (key-combo-execute-original " << "))
    ;; " < " should be bound in flex-autopair
    ("<=" . " <= ")
    ;; ("<?" . "<?`!!'?>");; for what?
    ("<<=" . " <<= ");; bit shift for c
    ("<-" . " <- ")
    ("<!" . "<!-- `!!' -->");; for html comment
    ("|"  . (" | " " || "));; bit OR and OR for c
    ;;ToDo: ruby block
    ("|=" . " |= ");; for c
    ("||=" . " ||= ")                   ; for ruby
    ;; ("/" . (" / " "// " "/`!!'/")) ;; devision,comment start or regexp
    ("/" . (key-combo-execute-original))
    ("/ SPC" . " / ")
    ("/=" . " /= ")
    ("*/" . "*/")
    ("/*" . "/* `!!' */")
    ("/* RET" . "/*\n`!!'\n*/");; add *? m-j
    ;; ("/* RET" . "/*\n*`!!'\n*/");; ToDo:change style by valiable
    ("{" . (key-combo-execute-original))
    ("{ RET" . "{\n`!!'\n}")
    )
  "Default binding which enabled by `key-combo-common-mode-hooks'"
  :group 'key-combo)

;; rhtml-mode
;; <% rinari-insert-erb-skeleton
;; (key-combo-define-local "<%" 'rinari-insert-erb-skeleton)
;; (key-combo-define-local ":" ":")

(defcustom key-combo-org-default
  '(("C-a" . (org-beginning-of-line
              beginning-of-buffer
              key-combo-return));;back-to-indentation
    ("C-e" . (org-end-of-line
              end-of-buffer
              key-combo-return))
    )
  "Default binding which enabled by `org-mode-hook'"
  :group 'key-combo)

(defcustom key-combo-pointer-default
  '(("*" . ("*" "**" "***"))
    ("&" . ("&" "&&" "&&&"))
    ("->" . "->"))
  "Default binding for c-mode,c++-mode,objc-mode"
  :group 'key-combo)

(defcustom key-combo-perl-default
  '(("$" . (key-combo-execute-original))
    ("@" . (key-combo-execute-original))
    ("%" . (key-combo-execute-original))
    ("&" . (key-combo-execute-original))
    ("*" . (key-combo-execute-original))
    ("->" . "->"))
  "Default binding for c-mode,c++-mode,objc-mode"
  :group 'key-combo)

;;;###autoload
(defmacro key-combo-define-hook (hooks name keys)
  ;; fix me:name to real name (not symbol)
  ;; :hooks to quote
  ;; don't use macro?
  `(progn
     (defun ,(nth 1 name) ()
       (key-combo-load-default-1 (current-local-map) ,keys)
       )
     (key-combo-load-by-hooks ,hooks ,name)
     ))

;;;###autoload
(defun key-combo-load-default ()
  (interactive)
  (global-key-combo-mode t)
  (key-combo-load-default-1 (current-global-map)
                            key-combo-global-default)
  (key-combo-define-hook key-combo-common-mode-hooks
                         'key-combo-common-load-default
                         key-combo-common-default)
  (key-combo-define-hook key-combo-lisp-mode-hooks
                         'key-combo-lisp-load-default
                         key-combo-lisp-default)
  (key-combo-define-hook '(c-mode-hook c++-mode-hook)
                         'key-combo-pointer-load-default
                         key-combo-pointer-default)
  (key-combo-define-hook '(cperl-mode-hook perl-mode-hook)
                         'key-combo-pointer-load-default
                         key-combo-perl-default)
  (key-combo-define-hook 'objc-mode-hook
                         'key-combo-objc-load-default
                         (append key-combo-pointer-default
                                 '(("@"  . "@\"`!!'\""))))
  (key-combo-define-hook 'org-mode-hook
                         'key-combo-org-load-default
                         key-combo-org-default)
  (key-combo-define-hook '(html-mode-hook
                           css-mode-hook
                           javascript-mode-hook
                           js-mode-hook
                           makefile-mode-hook
                           js2-mode-hook)
                         'key-combo-property-default
                         '((":"  . ":")));; not work in erb
  ;; align is better for property?
  )

;; hooks function-name keys
(defun key-combo-load-by-hooks (hooks func)
  (let ((hooks (if (consp hooks) hooks (list hooks))))
    (dolist (hook hooks)
      (add-hook hook func t))
    ))

(defun key-combo-load-default-1 (map keys)
  (dolist (key keys)
    (key-combo-define map (key-combo-read-kbd-macro (car key)) (cdr key))))

(defadvice mac-handle-input-method-change (after key-combo-mac-handle-input-method-chang activate)
  (if (mac-input-source-is-ascii-capable)
      (progn
        (key-combo-mode-maybe))
    (key-combo-mode -1)
    (keyboard-quit)
    ))
;; please define message
;; print table

;; define key
;; define keys
(provide 'key-combo)
;;; key-combo.el ends here
