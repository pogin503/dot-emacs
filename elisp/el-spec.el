;;; el-spec.el --- ruby's rspec like syntax test frame work

;;-------------------------------------------------------------------
;;
;; Copyright (C) 2012 Yuuki Arisawa
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
;; URL: https://github.com/uk-ar/el-spec
;; Created: 4 July 2012
;; Version: 0.2
;; Keywords: test

;;; Commentary:

;; ########   Compatibility   ########################################
;;
;; Works with Emacs-23.2.1, 23.1.1

;; ########   Quick start   ########################################
;;
;; Add to your ~/.emacs
;;
;; (require 'el-spec)
;;
;; and write some test, for example
;;
;; (dont-compile
;;   (when (fboundp 'describe)
;;     (describe "description"
;;       (before
;;         (message "before common"))
;;       (after
;;         (message "after common\n"))
;;       (context "when 1"
;;         (before
;;           (message "before 1"))
;;         (after
;;           (message "after 1"))
;;         (it "test 1"
;;           (message "test 1")))
;;       (context "when 2"
;;         (before
;;           (message "before 2"))
;;         (after
;;           (message "after 2"))
;;         (it "test 2"
;;           (message "test 2")))
;;       )))
;;
;; output is like this.
;;
;; before common
;; before 1
;; test 1
;; after 1
;; after common
;;
;; before common
;; before 2
;; test 2
;; after 2
;; after common

;;; History:
;;
;; Revision 0.2 2012/07/18 21:06:41
;; * Add support to find example definition from ert test result.
;; * Add function to execute one example.
;;
;; Revision 0.1 2012/07/05 00:55:38
;; * First release
;;
;;; Bug:
;; (shared-examples ("examples for quote" :vars ((quote)))

(require 'ert)
(require 'cl)

(defmacro el-spec:around (&rest body)
  (push
   `(lambda (el-spec:example)
      ,@body)
   el-spec:full-context)
  nil)

(defmacro el-spec:before (&rest body)
  `(el-spec:around
    ,@body
    (funcall el-spec:example)
    ))

(defmacro el-spec:after (&rest body)
  ;; don't use around macro because of `el-spec:example' variable binding
  ;; use `el-spec:example' for test ability
  ;; (let ((example-sym (make-symbol "example")))
  ;;   (push
  ;;    `(lambda (,example-sym)
  ;;       (funcall ,example-sym)
  ;;       ,@body)
  ;;    el-spec:full-context))
  ;; nil)
  `(el-spec:around
    (funcall el-spec:example)
    ,@body
    ))

(defun el-spec:prepare-arglist (arglist)
  (declare (indent 1))
  (let ((arglist arglist))
    (typecase arglist
      (string (list arglist))
      (null (list nil))
      (cons
       (typecase (car-safe arglist)
         (string arglist)
         (null arglist)
         (keyword (push nil arglist))
         (t
          (error "%S is not string or list or nil" arglist))))
      (t
       (error "%S is not string or list or nil" arglist)))
    ))

(defun el-spec:compose (f g)
  `(lambda () (funcall (function ,g) (function ,f))))

(defconst el-spec:separator "\n")

(defmacro el-spec:it (arglist &rest body)
  (declare (indent 1))
  (destructuring-bind (&optional desc &key vars)
      (el-spec:prepare-arglist arglist)
    (lexical-let ((el-spec:full-context el-spec:full-context)
                  (el-spec:descriptions el-spec:descriptions))
      (push
       `(lambda () ,@body)
       el-spec:full-context)
      (when vars
        (push (format "%S" vars) el-spec:descriptions)
        (push el-spec:separator el-spec:descriptions)
        )
      (push (or desc (format "%S" body)) el-spec:descriptions)
      (let ((test-symbol (intern
                          (apply 'concat (reverse el-spec:descriptions)))))
        (when (ert-test-boundp test-symbol)
          (warn "test function \"%s\" already exist" test-symbol))
        `(el-spec:let ,vars
           (lexical-let ,(mapcar (lambda (var)
                                   `(,var ,var)) el-spec:vars)
             (ert-deftest ,test-symbol ()
               (funcall ,(reduce #'el-spec:compose
                                 el-spec:full-context))
               )
             ;; This hack allows `symbol-file' to associate `ert-deftest'
             ;; forms with files, and therefore enables `find-function' to
             ;; work with tests.  However, it leads to warnings in
             ;; `unload-feature', which doesn't know how to undefine tests
             ;; and has no mechanism for extension.
             ;; (push '(ert-deftest . ,name) current-load-list)
             ;; ',name))))
             )
           )
        ))))

(defmacro el-spec:context (arglist &rest body)
  (declare (indent 1))
  ;; typecase
  (destructuring-bind (desc &key vars) (el-spec:prepare-arglist arglist)
    (when (null desc)
      (error "%S does not have string description" arglist))
    `(let ((el-spec:full-context
            (if (boundp 'el-spec:full-context) el-spec:full-context nil))
           (el-spec:descriptions
            (if (boundp 'el-spec:descriptions) el-spec:descriptions nil))
           (el-spec:vars
            (if (boundp 'el-spec:vars) el-spec:vars nil)))
       (push ,desc el-spec:descriptions)
       (push el-spec:separator el-spec:descriptions)
       ;; fix?
       (el-spec:let ,vars
         ,@body
         )
       )
    ))

(defmacro describe (arglist &rest body)
  (declare (indent 1))
  ;; for failed test
  (makunbound 'el-spec:full-context)
  (makunbound 'el-spec:descriptions)
  (makunbound 'el-spec:vars)
  (el-spec:parse)
  `(let ((el-spec:full-context nil)
         (el-spec:descriptions nil)
         (el-spec:vars nil))
     ;; macrolet
     (letf (((symbol-function 'around) (symbol-function 'el-spec:around))
            ((symbol-function 'after) (symbol-function 'el-spec:after))
            ((symbol-function 'before) (symbol-function 'el-spec:before))
            ((symbol-function 'it) (symbol-function 'el-spec:it))
            ((symbol-function 'context) (symbol-function 'el-spec:context))
            ((symbol-function 'shared-context)
             (symbol-function 'el-spec:shared-context))
            ((symbol-function 'include-context)
             (symbol-function 'el-spec:include-context))
            ((symbol-function 'shared-examples)
             (symbol-function 'el-spec:shared-examples))
            ((symbol-function 'include-examples)
             (symbol-function 'el-spec:include-examples))
            )
       (el-spec:context ,arglist
         ,@body
         ))
     )
  )

(put 'it 'lisp-indent-function 1)
(put 'context 'lisp-indent-function 1)
(put 'before 'lisp-indent-function 0)
(put 'around 'lisp-indent-function 0)
(put 'after 'lisp-indent-function 0)
(put 'shared-context 'lisp-indent-function 1)
(put 'shared-examples 'lisp-indent-function 1)

(defmacro el-spec:let (varlist &rest body)
  (declare (indent 1))
  (mapc (lambda (element)
          (add-to-list
           'el-spec:vars (if (consp element) (car element) element)))
        varlist)
  `(let ,varlist
     ,@body
     )
  )

;; copy from el-expectations
(defun el-sepc:current-form-is-describe ()
  (save-excursion
    (forward-char)
    (beginning-of-defun)
    (looking-at "(describe\\|(.+(fboundp 'describe)\\|(dont-compile\n.*describe")))

(substitute-key-definition 'expectations-eval-defun 'eval-defun emacs-lisp-mode-map)
(substitute-key-definition 'expectations-eval-defun 'eval-defun lisp-interaction-mode-map)

(defvar el-spec:selection 'all
  ;; all or context
  )

(defun el-spec:toggle-selection ()
  (interactive)
  (cond
   ((eq el-spec:selection 'all)
    (setq el-spec:selection 'examples)
    (message "selection:examples")
    )
   ((eq el-spec:selection 'examples)
    (setq el-spec:selection 'all)
    (message "selection:all"))
   (t (warn "el-spec:selection is invalid"))))

(defadvice ert (around el-spec:ert-advice activate)
  (if (and (fboundp 'popwin:popup-buffer-tail)
           (not (interactive-p)))
      (let ((special-display-function 'popwin:popup-buffer-tail))
        ad-do-it
        (popwin:popup-buffer-tail "*ert*"))
    ad-do-it))

;; copy from ert
(defun ert-find-test-other-window (test-name)
  "Find, in another window, the definition of TEST-NAME."
  (interactive (list (ert-read-test-name-at-point "Find test definition: ")))
  (let ;; ((pop-up-windows t)
      ;;  (pop-up-frames t))
      ((display-buffer-reuse-frames t))
    (find-function-do-it test-name 'ert-deftest 'pop-to-buffer))
  );; 'switch-to-buffer-other-window

(defvar el-spec:first-time-p t)
(make-variable-buffer-local 'el-spec:first-time-p)

(defadvice eval-defun (around el-spec:eval-defun-advice activate)
  (if (not (and (interactive-p)
                (el-sepc:current-form-is-describe)))
      ad-do-it
    (ert-delete-all-tests)
    ;; for find-func
    ;; (assoc buffer-file-name load-history) is too slow...
    (if el-spec:first-time-p
        (progn
        (eval-buffer)
          (setq el-spec:first-time-p nil))
      ad-do-it
      )
    (case el-spec:selection
      ((all)
       (ert t))
      ((examples)
       (el-spec:execute-examples))
      (t (warn "el-spec:selection is invalid")))
    ))

(defun el-spec:eval-and-execute-all ()
  (interactive)
  (let ((el-spec:selection 'all))
    (call-interactively 'eval-defun)
    ))

(defun el-spec:eval-and-execute-examples ()
  (interactive)
  (let ((el-spec:selection 'examples))
    (call-interactively 'eval-defun)
    ))

(defmacro el-spec:defun-sexp (name)
  `(defun ,(intern (format "el-spec:%s" name)) (&optional arg)
     (condition-case err
         (progn
           (if (or (nth 3 (syntax-ppss));string
                   (nth 4 (syntax-ppss)));comment
               (goto-char (nth 8 (syntax-ppss)));; beginning
             )
           (,name arg))
       (scan-error
        ;;top level
        ))))

(el-spec:defun-sexp backward-sexp)
(el-spec:defun-sexp backward-up-list)
(el-spec:defun-sexp down-list)
(el-spec:defun-sexp forward-sexp)

(defun el-spec:re-position ()
  (if (or (nth 3 (syntax-ppss));string
          (nth 4 (syntax-ppss)));comment
      (goto-char (nth 8 (syntax-ppss)));; beginning
    ))

(defun el-spec:string-or-comment-p ()
  (or (nth 3 (syntax-ppss));string
      (nth 4 (syntax-ppss)));comment
    )

(defvar el-spec:example-tag nil)
;; Do not use buffer local variable.
;; Because find-definition-noselect can not find definition.

(defun el-spec:execute-examples ()
  (save-excursion
    (let ((test-name
           (save-excursion
             (forward-char)
             (let ((symbol (substring-no-properties
                            (or (thing-at-point 'symbol) ""))))
               (if (or (string= symbol "it")
                       (string= symbol "context")
                       (string= symbol "describe"))
                   (car-safe (rassoc (point) el-spec:example-tag)) nil)
               ))))
      (backward-char)
      (el-spec:re-position)
      (unless test-name
        (condition-case err
            (while (null test-name)
              (backward-up-list)
              (save-excursion
                (el-spec:down-list)
                (let ((symbol (substring-no-properties
                               (or (thing-at-point 'symbol) ""))))
                  (if (or (string= symbol "it")
                          (string= symbol "context")
                          (string= symbol "describe"))
                      (setq test-name
                            (car-safe (rassoc (point) el-spec:example-tag)))))
                ))
          (scan-error
           ;;top level
           )))
        (if test-name
            (ert (symbol-name test-name))
        (message "no example")
        ))
      ))

(defmacro el-spec:shared-context (arglist &rest body)
  (declare (indent 1))
  `(let ((el-spec:full-context nil)
         (el-spec:descriptions nil)
         (el-spec:vars nil))
     (el-spec:context ,arglist
       ,@body
       (set (intern (format "el-spec:context-%s"
                            (car (last el-spec:descriptions))))
            (list el-spec:full-context
                  el-spec:descriptions
                  el-spec:vars))
       )))

(defmacro el-spec:include-context (desc)
  ;; macro for set el-spec:full-context
  (let ((context (intern (format "el-spec:context-%s" desc))))
    `(progn
       (setq el-spec:full-context (append (car ,context)
                                          el-spec:full-context))
       ;; (setq el-spec:descriptions (append (nth 1 (symbol-value context))
       ;;                                    el-spec:descriptions))
       (setq el-spec:vars (append (nth 2 ,context)
                                  el-spec:vars))
       )
    ))

(defmacro el-spec:shared-examples (arglist &rest body)
  (declare (indent 1))
  (destructuring-bind (desc &key vars) (el-spec:prepare-arglist arglist)
    (when (null desc)
      (error "%S does not have string description" arglist))
    `(setq ,(intern (format "el-spec:examples-%s" desc))
           (lambda ()
             ;; (let ((el-spec:full-context nil)
             ;;      (el-spec:descriptions nil)
             ;;      (el-spec:vars nil))
             (el-spec:context ,arglist
               ,@body
               )));; )
    ))

(defun el-spec:get-description ()
  (save-excursion
    (forward-sexp)
    (forward-sexp)
    (let ((arglist (read (substring-no-properties (thing-at-point 'sexp)))))
      (destructuring-bind (desc &key vars)
          (el-spec:prepare-arglist arglist)
        (when (null desc)
          (error "%S does not have string description" arglist))
        (list el-spec:separator desc)))))

(defun el-spec:get-description-for-it ()
  (save-excursion
    (let ((descriptions))
      (destructuring-bind (symbol arglist &rest body)
          (read (substring-no-properties (thing-at-point 'list)))
        (destructuring-bind (desc &key vars)
            (el-spec:prepare-arglist arglist)
          (when vars
            (push (format "%S" vars) descriptions)
            (push el-spec:separator descriptions)
            )
          (push (or desc (format "%S" body)) descriptions)
          ))
      descriptions
      )))

(defun el-spec:first-element ()
  (save-excursion
    (el-spec:re-position)
    ;; (car (read (substring-no-properties (or (thing-at-point 'list) ""))))
    (el-spec:backward-up-list);;top-level
    (el-spec:down-list)
    (el-spec:forward-sexp)
    (substring-no-properties (thing-at-point 'symbol))
    )
  )

(defun el-spec:parse ()
  (save-excursion
    (goto-char (point-min))
    (while (and (re-search-forward "describe" (point-max) t)
                (not (el-spec:string-or-comment-p)))
      ;; in case for
      ;; (;; comment
      ;;  describe
      (when (string= (el-spec:first-element) "describe")
        (backward-sexp)
        (push (cons (intern (apply 'concat (reverse (el-spec:get-description))))
                    (point))
              el-spec:example-tag)
        (el-spec:parse-1 (el-spec:get-description))
        )
      )
    ))

(defun el-spec:parse-1 (descriptions)
  (condition-case err
      (while t
        (condition-case err
            (save-excursion
              (down-list)
              (let ((symbol (substring-no-properties
                             (or (thing-at-point 'symbol) ""))))
                (cond
                 ((equal symbol "context")
                  (let ((desc (append (el-spec:get-description) descriptions)))
                    (push (cons (intern (apply 'concat (reverse desc)))
                                (point)) el-spec:example-tag)
                    (el-spec:parse-1 desc)
                    ))
                 ((equal symbol "it")
                  (let ((test-name
                         (intern
                          (apply
                           'concat
                           (reverse
                            ;; (el-spec:parse-1
                            (append
                             (el-spec:get-description-for-it)
                             descriptions))))));; )
                    (push (cons test-name (point)) el-spec:example-tag))
                  )
                 ((equal symbol "shared-examples")
                  ;; (message "share")
                  )
                 (t
                  (el-spec:parse-1 descriptions))
                 )
                )
              )
          (scan-error
           ;; bottom
           ))
        (forward-list)
        (el-spec:parse-1 descriptions)
        )
    (scan-error
     ;; end of list
     )
    )
  descriptions
  )

(defmacro el-spec:include-examples (arglist)
  (cond
   ((stringp arglist)
    (setq arglist (list arglist)))
   ((not (consp arglist))
    (error "%S is not string or list" arglist)
    ))
  (destructuring-bind (desc &key vars) arglist
    (let ((context
              (intern (format "el-spec:examples-%s" desc))))
      `(el-spec:let ,vars
         (funcall ,context)
         )
      )))

;; (setq cmd "=")を忘れたとき

(defun el-spec:eval-buffer ()
  (interactive)
  (ert-delete-all-tests)
  (eval-buffer)
  (ert t)
  )

(defadvice find-definition-noselect
  (after el-spec:find-definition-noselect-advice activate)
  (destructuring-bind (symbol type &optional file) (ad-get-args 0)
    (when (and (null (cdr-safe ad-return-value))
               (eq type 'ert-deftest))
      (let ((pos (assoc-default symbol el-spec:example-tag)))
        (if pos
            (setq ad-return-value
                  (cons (car-safe ad-return-value) pos))
          (error "Can not find function. shared-example?"))))))

;;print test name
(defun ert-insert-test-name-button (test-name)
  "Insert a button that links to TEST-NAME."
  (insert-text-button (format "%s" test-name)
                      :type 'ert--test-name-button
                      'ert-test-name test-name))

(provide 'el-spec)
;;; el-spec.el ends here
