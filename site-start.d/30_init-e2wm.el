;;;;  30_init-e2wm.el --
;; -*- Mode: Emacs-Lisp -*-

;; Created: Tue Jul 19 03:56:19 2011
;; Last-Updated: Time-stamp: <2011-07-19 15:29:18 ogin>
;; URL:
;; Keywords:
;; Compatibility:
;;

;;; Commentary:
;;
;;
;;

;; Licence
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;
;;; Code:
(when (autoload-if-found 'e2wm:start-management "e2wm" nil t)
  (global-set-key (kbd "M-+") 'e2wm:start-management))

(lazyload (e2wm:start-management) "e2wm"
          (require 'e2wm)

          ;; for 1024x768
          (setq e2wm:c-code-recipe
                '(| (:left-max-size 35)
                    (- (:upper-size-ratio 0.7)
                       (- (:upper-size-ratio 0.6)
                          files imenu)
                       history)
                    (- (:upper-size-ratio 0.7)
                       main sub)))

          (req e2wm-config)

          (req e2wm-vcs)
          )
          ;; 30_init-e2wm.el ends here
