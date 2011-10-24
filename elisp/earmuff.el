;;; earmuff.el

;; Copyright (C) 2011 by valvallow

;; Author: valvallow <valvalloooooooooow atmark gmail.com>
;; blog (japanese) : http://valvallow.blogspot.com/
;; Last modified: Time-stamp: <Tue Mar 01 21:11:24  2011>

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
;; usage
;;
;; (require 'earmuff)
;; (define-key global-map [f11] (emf:cycle-earmuff-gen '((?" ?")(?* ?*)(?+ ?+))))
;; (define-key global-map (kbd "M-<f11>") (emf:earmuff-and-move-next-gen '(?" ?")))
;;
;; or
;;
;; (require 'earmuff)
;; (add-to-list 'insert-pair-alist '(?" ?"))
;; (add-to-list 'insert-pair-alist '(?* ?*))
;; (add-to-list 'insert-pair-alist '(?+ ?+))
;; (define-key global-map [f11] (emf:cycle-earmuff-gen))
;; (define-key global-map (kbd "M-<f11>") (emf:earmuff-and-move-next-gen '(?" ?")))
;;

(eval-when-compile (require 'cl))

(defun* emf:earmuff? (str &optional (pair-src insert-pair-alist))
  (unless (string= str "")
    (let ((head (substring str 0 1))
          (tail (substring str (- (length str) 1))))
      (member (mapcar #'string-to-char (list head tail))
              pair-src))))

(defun emf:forward-sexp-string ()
  (buffer-substring
   (save-excursion (forward-sexp)
                   (point))
   (point)))

(defun* emf:cycle-earmuff-gen (&optional (pair-src insert-pair-alist))
  (lexical-let ((index 0)
                (pair-src pair-src))
    #'(lambda ()
        (interactive)
        (unless (eq this-command real-last-command)
          (setq index 0))
        (let ((str (emf:forward-sexp-string)))
          (when (emf:earmuff? str pair-src)
            (delete-pair))
          (if (<= (length pair-src) index)
              (setq index 0)
            (save-excursion
              (apply #'insert-pair (not (string= str ""))
                     (nth index pair-src))
              (incf index)))))))

(defun emf:earmuff-and-move-next-gen (charset)
  (lexical-let ((src charset))
    #'(lambda ()
        (interactive)
        (apply #'insert-pair t src)
        (forward-sexp)
        (forward-char))))

(provide 'earmuff)