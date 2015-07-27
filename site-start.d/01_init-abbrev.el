;;; 01_init-abbrev.el --- 01_init-abbrev.el -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;; This program is free software
;;; Code:

(define-abbrev-table 'java-mode-abbrev-table
  '(("psv" "public static void main(String[] args) {" nil 0)
    ("sopl" "System.out.println" nil 0)
    ("sop" "System.out.printf" nil 0)
    ("sysout" "System.out.printf" nil 0)))

(provide '01_init-abbrev)
;;; 01_init-abbrev.el ends here
