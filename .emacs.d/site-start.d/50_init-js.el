(setq js-indent-level 4)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook-fn 'js2-mode-hook
			 (setq c-toggle-auto-newline 1)
			 (setq c-toggle-auto-state 1)
			 (setq c-toggle-auto-hungry-state 1)
			 ;; (require 'flymake-jsl)
			 ;; (setq flymake-check-was-interrupted t)
			 (when (req org nil t)
			   (define-key js2-mode-map "\C-a" 'seq-home)
			   (define-key js2-mode-map "\C-e" 'seq-end)))

;; @see http://d.hatena.ne.jp/speg03/20091011/1255244329
;; fixing indentation
;; @see http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
(autoload 'espresso-mode "espresso")

(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (espresso--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ espresso-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-indent-sexp ()
  (interactive)
  (save-restriction
    (save-excursion
      (widen)
      (let* ((inhibit-point-motion-hooks t)
             (parse-status (syntax-ppss (point)))
             (beg (nth 1 parse-status))
             (end-marker (make-marker))
             (end (progn (goto-char beg) (forward-list) (point)))
             (ovl (make-overlay beg end)))
        (set-marker end-marker end)
        (overlay-put ovl 'face 'highlight)
        (goto-char beg)
        (while (< (point) (marker-position end-marker))
          ;; don't reindent blank lines so we don't set the "buffer
          ;; modified" property for nothing
          (beginning-of-line)
          (unless (looking-at "\\s-*$")
            (indent-according-to-mode))
          (forward-line))
        (run-with-timer 0.5 nil '(lambda(ovl)
                                   (delete-overlay ovl)) ovl)))))

(defun my-js2-mode-hook ()
  (require 'espresso)
  (setq espresso-indent-level 4
        indent-tabs-mode nil
        c-basic-offset 4)
  (c-toggle-auto-newline 0)
  (c-toggle-hungry-state 1)
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  ; (define-key js2-mode-map [(meta control |)] 'cperl-lineup)
  (define-key js2-mode-map "\C-\M-\\"
    '(lambda()
       (interactive)
       (insert "/* -----[ ")
       (save-excursion
         (insert " ]----- */"))
       ))
  (define-key js2-mode-map "\C-m" 'newline-and-indent)
  ; (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  ; (define-key js2-mode-map [(control d)] 'c-electric-delete-forward)
  (define-key js2-mode-map "\C-\M-q" 'my-indent-sexp)
  (if (featurep 'js2-highlight-vars)
      (js2-highlight-vars-mode))
  (message "My JS2 hook"))

(add-hook 'js2-mode-hook 'my-js2-mode-hook)


;; ac-my-completion for auto-complete
(defun ac-my-completion-list-in-files (files &optional keywords-variable)
  "create completion list for auto-complete"
  (let ((ksymbol keywords-variable) keywords)
    (cond
     ((stringp files)
      (setq files (list files)))
     ((stringp keywords-variable)
      (setq ksymbol (intern keywords-variable))))
    (if (not (boundp ksymbol))
        (progn
          (dolist (file files)
            (let ((buffer (find-file-noselect file)) filename)
              (setq filename (file-name-nondirectory file))
              (with-current-buffer buffer
                (rename-buffer
                 (concat " *" filename "*") t)
                (if (< (buffer-size) 131072) ; 128Kb
                    (save-excursion
                      (goto-char (point-min))
                      (while (re-search-forward "^[^;]\\(\\s_\\|\\sw\\)+\\b$" nil t)
                        (let ((candidate (match-string-no-properties 0)))
                          (if (not (member candidate keywords))
                              (push candidate keywords))))
                      (setq keywords (nreverse keywords)))))))
          ;; (sort keywords #'(lambda (a b) (> (length a) (length b))))
          (if ksymbol
              (set-default ksymbol keywords))
          (message (concat "Building ac-source keywords(" (symbol-name ksymbol) ")...done."))
          keywords))))

(defun ac-my-completion-files (files mode-name &optional set-hook)
  "set completion list to mode-variable for auto-complete"
  (lexical-let
      ((--ac-source (intern (concat "ac-source-" mode-name)))
       (--ac-source-keywords (intern (concat "ac-source-" mode-name "-keywords")))
       (--mode-hook (intern (concat mode-name "-mode-hook")))
       (--files files))
    (set-default --ac-source
                 (list (cons 'candidates
                             (lambda ()
                               (all-completions ac-prefix (symbol-value --ac-source-keywords))))))
    (if set-hook
        (add-hook --mode-hook
                  (lambda()
                    (ac-my-completion-list-in-files --files --ac-source-keywords)
                    (make-local-variable 'ac-sources)
                    (setq ac-sources (append ac-sources (list --ac-source)))))
      (ac-my-completion-list-in-files --files --ac-source-keywords))))

;; 以下をモード毎に設定
(ac-my-completion-files "~/.emacs.d/plugins/ac-dict/javascript+DOM" "js2" t)