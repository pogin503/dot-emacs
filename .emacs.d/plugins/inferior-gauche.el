;; Subject: inferior-gauche.el
;; History: 2005-07-27 22:27:56 << 2003-07-16 11:09:47
;; ----

(setq debug-on-error t)

(require 'scheme)
;; (require 'info-look)

;; ===========================================
;; TODO Memo

;; ===========================================
;; User Variables

(defvar igosh-gosh "gosh")
(defvar igosh-gosh-args "")
(defvar igosh-decoding-system 'euc-jp)
(defvar igosh-encoding-system 'euc-jp)

;; ===========================================
;; Internal Variables

(defvar igosh-process-name "gosh")
(defvar igosh-error-regexp (concat "^" (regexp-quote "*** ERROR: ")))
(defvar igosh-gosh-prompt "gosh> ")
(defvar igosh-error-buffer-name " * IGOSH ERROR")
(defvar igosh-temp-buffer-name " * IGOSH TEMP")
(defvar igosh-processes nil)

(defvar igosh-mode-line-format
  (if (member 'mode-line-process default-mode-line-format)
      (append
       (reverse (cdr (member 'mode-line-process (reverse default-mode-line-format))))
       '((:eval (igosh-mode-line-process)))
       (member 'mode-line-process default-mode-line-format))
    default-mode-line-format))

(defvar igosh-font-lock-keywords
  (append
   scheme-font-lock-keywords-1
   scheme-font-lock-keywords-2
   `((,(format "(%s\\>"
               (regexp-opt
                '("define-reader-ctor" "define-values" "define-constant"
                  "define-in-module" "and-let*" "begin0"
                  "call-with-client-socket" "call-with-input-conversion" "call-with-input-process"
                  "call-with-input-string" "call-with-iterator" "call-with-output-conversion"
                  "call-with-output-string" "call-with-temporary-file" "call-with-values"
                  "dolist" "dotimes" "if-match" "let*-values" "let-args" "let-keywords*"
                  "let-match" "let-optionals*" "let-syntax" "let-values" "let1" "let/cc"
                  "letrec-syntax" "make" "multiple-value-bind" "parameterize" "parse-options"
                  "receive" "rxmatch-case" "rxmatch-cond" "rxmatch-if" "rxmatch-let"
                  "syntax-rules" "unless" "until" "when" "while" "with-builder"
                  "with-error-handler" "with-error-to-port" "with-input-conversion"
                  "with-input-from-port" "with-input-from-process" "with-input-from-string"
                  "with-iterator" "with-module" "with-output-conversion"
                  "with-output-to-port" "with-port-locking" "with-string-io"
                  "with-time-counter") t))
      1 font-lock-keyword-face)
     (,(format "(%s\\>"
               (regexp-opt
                '("error" "errorf" "syntax-error" "syntax-errorf") t)) 1 font-lock-warning-face)
     ("\\<<>\\>" . font-lock-builtin-face))))

(mapcar '(lambda (s)
           (put (car s) 'scheme-indent-function (cdr s)))
        '(
          (if . 1)
          ;; From "Gauche:EditingWithEmacs" in WiLiKi
          (and-let* . 1) (begin0 . 0) (call-with-client-socket . 1)
          (call-with-input-conversion . 1) (call-with-input-file . 1)
          (call-with-input-process . 1) (call-with-input-string . 1)
          (call-with-iterator . 1) (call-with-output-conversion . 1)
          (call-with-output-file . 1) (call-with-output-string . 0)
          (call-with-temporary-file . 1) (call-with-values . 1)
          (dolist . 1) (dotimes . 1) (if-match . 2) (let*-values . 1)
          (let-args . 2) (let-keywords* . 2) (let-match . 2) (let-optionals* . 2)
          (let-syntax . 1) (let-values . 1) (let1 . 2) (let/cc . 1)
          (letrec-syntax . 1) (make . 1) (multiple-value-bind . 2) (parameterize . 1)
          (parse-options . 1) (receive . 2) (rxmatch-case . 1) (rxmatch-cond . 0)
          (rxmatch-if  . 4) (rxmatch-let . 2) (syntax-rules . 1) (unless . 1) (until . 1)
          (when . 1) (while . 1) (with-builder . 1) (with-error-handler . 0)
          (with-error-to-port . 1) (with-input-conversion . 1) (with-input-from-port . 1)
          (with-input-from-process . 1) (with-input-from-string . 1) (with-iterator . 1)
          (with-module . 1) (with-output-conversion . 1) (with-output-to-port . 1)
          (with-port-locking . 1) (with-string-io . 1) (with-time-counter . 1)))

(defvar igosh-syntax-table (let ((syntax (copy-syntax-table scheme-mode-syntax-table)))
                             (modify-syntax-entry ?\| "  2b3" syntax)
                             (modify-syntax-entry ?# "_ p14b" syntax)
                             syntax))

;;(defvar inferior-gauche-mode-map (let ((map (make-sparse-keymap)))
(setq inferior-gauche-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map scheme-mode-map)
        (define-key map [?\C-\M-x] 'igosh-eval-define)
        (define-key map [?\C-x ?\C-e] 'igosh-eval-last-sexp)
        (define-key map [?\C-j] 'igosh-eval-print-last-sexp)
        (define-key map [?\C-c ?\C-s] 'igosh-start-process)
        (define-key map [?\M-\C-i] 'igosh-complete-symbol)
        (define-key map [?\C-c ?\C-d] 'igosh-insert-debug-print)
        (define-key map [?\C-c ?\C-c] 'igosh-interrupt-process)
        (define-key map [?\C-c ?\C-q] 'igosh-exit-process)
        (define-key map [?\C-c ?\C-v] 'igosh-eval-buffer)
        (define-key map [?\C-c ?\C-i] 'igosh-input)
        map))

;; ===========================================
;; Main Functions

(define-derived-mode inferior-gauche-mode scheme-mode
  "iGosh" "Major mode for Gauche."
  (set-syntax-table igosh-syntax-table)
  (setq mode-line-format igosh-mode-line-format
        comment-start ";;"
        font-lock-defaults
        `(,igosh-font-lock-keywords
          nil t (("+-*/.<>=!?$%_&~^:" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun)))
  (set (make-local-variable 'igosh-process) nil)
  (igosh-cleanup)
  (if (null igosh-processes)
      (igosh-start-new-process)
    (setq igosh-process (car igosh-processes))))

(defun igosh-cleanup ()
  ;; remove killed processes from 'igosh-processes'.
  (let ((procs (mapcar
                (lambda (proc)
                  (if (igosh-process-live-p proc)
                      proc
                    (kill-buffer (process-buffer (car igosh-processes)))
                    nil))
                igosh-processes)))
    (setq igosh-processes (delete nil procs))))

(defun igosh-mode-line-process ()
  (if (eq major-mode 'inferior-gauche-mode)
      (progn
        (igosh-cleanup)
        (if (igosh-process-live-p)
            (if (= 1 (length igosh-processes))
                (format " [%s]"
                        (process-status igosh-process))
              (format " [%s: %s]" (process-name igosh-process)
                      (process-status igosh-process)))
          " [no process]"))
    ""))

(defun igosh-process-live-p (&optional proc)
  (let ((proc (or proc igosh-process)))
    (and proc (member (process-status proc)
                      '(run stop)))))

(defmacro when-igosh-process-alive (&rest body)
  `(if (igosh-process-live-p)
       (progn ,@body)
     (message "No process running.")))

(defun igosh-start-process (uarg)
  (interactive "P")
  (igosh-cleanup)
  (if (or uarg (igosh-process-live-p))
      (igosh-select-process)
    (igosh-start-new-process)))

(defun igosh-select-process ()
  (let* ((comp-alist
          ;; ((proc-name . proc) ...)
          (mapcar (lambda (proc)
                    (cons (process-name proc) proc))
                  igosh-processes))
         (proc-name (completing-read "Process Name: " comp-alist
                                     nil nil
                                     (if (igosh-process-live-p)
                                         (process-name igosh-process)
                                       (caar comp-alist)))))
    (if (assoc proc-name comp-alist)
        (setq igosh-process (cdr (assoc proc-name comp-alist)))
      (igosh-start-new-process proc-name))))

(defun igosh-start-new-process (&optional pname)
  (let* ((proc-name (or pname igosh-process-name))
         (process-connection-type t)
         (buf (generate-new-buffer igosh-temp-buffer-name))
         (proc (if (and igosh-gosh-args (< 0 (length igosh-gosh-args)))
                   (start-process proc-name buf
                                  igosh-gosh igosh-gosh-args)
                 (start-process proc-name buf igosh-gosh))))
    (setq igosh-processes (cons proc igosh-processes))
    (setq igosh-process proc)
    (process-send-string proc igosh-apropos)
    (set-process-filter proc 'igosh-filter)
    (catch 'igosh-done
      ;; initial gosh>
      (while (igosh-process-live-p proc)
        (accept-process-output proc)))
    (catch 'igosh-done
      ;; igosh-apropos
      (while (igosh-process-live-p proc)
        (accept-process-output proc)))
    (with-current-buffer buf
      (erase-buffer))
    (set-process-sentinel proc 'igosh-process-msg)
    (set-process-coding-system proc igosh-decoding-system igosh-encoding-system)
    (process-kill-without-query proc nil)
    (message "%s: start" (process-name proc))))

(defun igosh-interrupt-process ()
  (interactive)
  (when-igosh-process-alive
   (interrupt-process igosh-process)))

(defun igosh-exit-process ()
  (interactive)
  (when-igosh-process-alive
   (process-send-string igosh-process "(exit)\n")))

(defun igosh-process-msg (proc event)
  (message "%s: %s" (process-name proc)
           (igosh-delete-last-newlines event)))

(defun igosh-delete-last-newlines (string)
  (while (and (< 0 (length string)) (= ?\n (aref string (- (length string) 1))))
    (setq string (substring string 0 (- (length string) 1))))
  string)

(defun igosh-insert-debug-print (arg)
  (interactive "P")
  (if arg
      (save-match-data
        (save-excursion
          (save-restriction
            (end-of-defun)
            (let ((end (point)))
              (beginning-of-defun)
              (narrow-to-region (point) end)
              (replace-string "#?=" "")))))
    (insert "#?=")))

(defun igosh-filter (proc out)
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert out)
    (let ((last-line (buffer-substring (line-beginning-position)
                                       (line-end-position)))
          (second-last-line-end-point (max (- (line-beginning-position) 1)
                                           (point-min))))
      (when (string= last-line igosh-gosh-prompt)
        (let ((output (buffer-substring (point-min) second-last-line-end-point)))
          (erase-buffer)
          (save-match-data
            (if (string-match igosh-error-regexp output)
                (progn
                  (igosh-error proc output)
                  (condition-case ()
                      (throw 'igosh-done nil)
                    (no-catch nil)))
              (condition-case ()
                  (throw 'igosh-done output)
                (no-catch (princ output))))))))))

;; ===========================================
;; Eval

(defun igosh-eval-print-last-sexp ()
  (interactive)
  (when-igosh-process-alive
   (let ((end (point))
         (beg (save-excursion
                (backward-sexp 1)
                (point))))
     (process-send-string igosh-process
                          (concat (buffer-substring-no-properties beg end) "\n"))
     (let ((output (catch 'igosh-done
                     (while (igosh-process-live-p igosh-process)
                       (accept-process-output igosh-process)))))
       (when (stringp output)
         (insert "\n" output "\n"))))))

(defun igosh-eval-last-sexp ()
  (interactive)
  (when-igosh-process-alive
   (let ((end (point))
         (beg (save-excursion
                (backward-sexp 1)
                (point))))
     (process-send-string igosh-process
                          (concat (buffer-substring-no-properties beg end) "\n")))))

(defun igosh-eval-region (beg end)
  (interactive "r")
  (when-igosh-process-alive
   (process-send-string igosh-process
                        (concat "(begin\n"
                                (buffer-substring-no-properties beg end)
                                "\n)\n"))))

(defun igosh-eval-buffer ()
  (interactive)
  (save-restriction
    (widen)
    (igosh-eval-region (point-min) (point-max))))

(defun igosh-eval-define ()
  (interactive)
  (save-excursion
    (igosh-eval-region (save-excursion (beginning-of-defun)
                                       (point))
                       (save-excursion (end-of-defun)
                                       (point)))))

;; ===========================================
;; Error

(defun igosh-error (porc error-string)
  (let ((err-buf (generate-new-buffer igosh-error-buffer-name))
        (winconf (current-window-configuration)))
    (set-buffer err-buf)
    (toggle-read-only -1)
    (erase-buffer)
    (insert error-string)
    (goto-char (point-min))
    (set (make-local-variable 'igosh-winconf) winconf)
    (toggle-read-only 1)
    (select-window (display-buffer err-buf))
    (use-local-map
     (let ((map (make-sparse-keymap)))
       (set-keymap-parent map (current-local-map))
       (define-key map [?q] 'igosh-quit-error-message)
       map))
    (message "q: quit.")))

(defun igosh-quit-error-message ()
  (interactive)
  (let ((winconf igosh-winconf))
    (kill-buffer (current-buffer))
    (when (window-configuration-p winconf)
      (set-window-configuration winconf))))

;; ===========================================
;; Completion

(defun igosh-complete-symbol ()
  (interactive)
  (when-igosh-process-alive
   (let ((end (point))
         (beg (max (line-beginning-position)
                   (save-excursion
                     (backward-sexp 1)
                     (point)))))
     (unless (= beg end)
       (process-send-string igosh-process
                            (format "(igosh-apropos \"%s\")\n"
                                    (buffer-substring-no-properties beg end)))
       (let ((output (catch 'igosh-done
                       (while (igosh-process-live-p igosh-process)
                         (accept-process-output igosh-process)))))
         (when (stringp output)
           (igosh-complete-symbol-body output)))))))

(defun igosh-complete-symbol-body (string)
  (let* ((collection (read string))
         (end (point))
         (beg (max (line-beginning-position)
                   (save-excursion
                     (backward-sexp 1)
                     (point))))
         (orig (buffer-substring beg end))
         (comp (try-completion orig collection))
         (comp-buff "*Completions*"))
    (when (stringp comp)
      (if (string= orig comp)
          (if (and (eq this-command last-command)
                   (window-live-p (get-buffer-window comp-buff)))
              (let ((win (selected-window)))
                (select-window (get-buffer-window comp-buff))
                (condition-case ()
                    (scroll-up)
                  (error (goto-char (point-min))))
                (select-window win))
            (with-output-to-temp-buffer comp-buff
              (display-completion-list
               (all-completions orig collection))))
        (delete-region beg end)
        (insert comp)))))

;; (defvar igosh-apropos
(setq igosh-apropos
      "(define (igosh-apropos string)
  (define (uniq rist)
    (let lp ((l rist)
             (val '()))
      (if (null? l)
        val
        (if (memq (car l) val)
          (lp (cdr l) val)
          (lp (cdr l) (cons (car l) val))))))

  (define modules
    (uniq (apply append (map module-precedence-list
                                 (cons (current-module)
                                       (module-imports (current-module)))))))

  (define matched '())

  (define regexp (string->regexp (string-append \"^\" (regexp-quote string))))

  (define (finder symbol)
    (let1 str (symbol->string symbol)
      (when (regexp str)
        (set! matched (cons str matched)))))

  (for-each (lambda (mod)
              (let1 exports (module-exports mod)
                (for-each finder (if (and (list? exports)
                                          (not (null? exports)))
                                   exports
                                   (hash-table-keys (module-table mod))))))
            modules)
  (map list (sort (uniq matched))))
")

;; ===========================================
;; Inputter

(defun igosh-input ()
  (interactive)
  (when-igosh-process-alive
   (let ((proc igosh-process)
         (winconf (current-window-configuration)))
     (display-buffer (process-buffer proc))
     (unwind-protect
         (let ((str (read-from-minibuffer (concat (process-name proc) "> "))))
           (set-buffer (process-buffer proc))
           (erase-buffer)
           (process-send-string proc (concat str "\n")))
       (set-window-configuration winconf)))))

;; ===========================================
;; Info

;; If info files are gzipped.
;; (auto-compression-mode 1)

;; ;; Manual (English)
;; (eval-after-load
;;     "info-look"
;;   '(info-lookup-add-help
;;     ;; For
;;     ;;  info-complete-symbol (to complete a symbol using the info)
;;     ;;  info-lookup-symbol   (to look up a symbol in the info)
;;     :topic 'symbol
;;     :mode  'inferior-gauche-mode
;;     :regexp "[^()'\" \t\n]+"
;;     :ignore-case nil
;;     :doc-spec '(("(gauche-refe.info)Function and Syntax Index" nil
;;                  "^[ \t]+-- [^:]+:[ \t]*" nil)
;;                 ("(gauche-refe.info)Module Index" nil
;;                  "^[ \t]+-- [^:]+:[ \t]*" nil)
;;                 ("(gauche-refe.info)Class Index" nil
;;                  "^[ \t]+-- [^:]+:[ \t]*" nil)
;;                 ("(gauche-refe.info)Variable Index" nil
;;                  "^[ \t]+-- [^:]+:[ \t]*" nil))
;;     :parse-rule  nil
;;     :other-modes nil))

;; Manual (Japanese)
(eval-after-load
    "info-look"
  ;; Emacs Info can't handle hoge:huga style e.g. ssax:xml->sxml.
  '(info-lookup-add-help
    ;; For
    ;;  info-complete-symbol (to complete a symbol using the info)
    ;;  info-lookup-symbol   (to look up a symbol in the info)
    :topic 'symbol
    :mode  'inferior-gauche-mode
    :regexp "[^()'\" \t\n]+"
    :ignore-case nil
    :doc-spec '(("(gauche-refj.info)Index - 手続きと構文索引" nil
                 "^[ \t]+-- [^:]+:[ \t]*" nil)
                ("(gauche-refj.info)Index - モジュール索引"   nil
                 "^[ \t]+-- [^:]+:[ \t]*" nil)
                ("(gauche-refj.info)Index - クラス索引"      nil
                 "^[ \t]+-- [^:]+:[ \t]*" nil)
                ("(gauche-refj.info)Index - 変数索引"        nil
                 "^[ \t]+-- [^:]+:[ \t]*" nil))
    :parse-rule  nil
    :other-modes nil))

(provide 'inferior-gauche)
;;; inferior-gauche.el ends here