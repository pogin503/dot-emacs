;;; 31_init-dired --- 31_init-dired
;; This program is free software
;;; Commentary:
;;; Code:

(require 'popwin)

(eval-after-load "dired"
  '(progn
     ;;@see http://www.bookshelf.jp/soft/meadow_25.html
     (defun my-dired-advertised-find-file ()
       (interactive)
       (let ((kill-target (current-buffer))
             (check-file (dired-get-filename)))
         (funcall 'dired-advertised-find-file)
         (if (file-directory-p check-file)
             (kill-buffer kill-target))))

     (defun my-dired-up-directory (&optional other-window)
       "Run dired on parent directory of current directory.
Find the parent directory either in this buffer or another buffer.
Creates a buffer if necessary."
       (interactive "P")
       (let* ((dir (dired-current-directory))
              (up (file-name-directory (directory-file-name dir))))
         (or (dired-goto-file (directory-file-name dir))
             ;; Only try dired-goto-subdir if buffer has more than one dir.
             (and (cdr dired-subdir-alist)
                  (dired-goto-subdir up))
             (progn
               (if other-window
                   (dired-other-window up)
                 (progn
                   (kill-buffer (current-buffer))
                   (dired up))
                 (dired-goto-file dir))))))

     (defface face-file-edited-today
       '((((class color)
           (background dark))
          (:foreground "GreenYellow"))
         (((class color)
           (background light))
          (:foreground "magenta"))
         (t
          ())) nil)

     (defvar face-file-edited-today
       'face-file-edited-today)
     ;; (defun my-dired-today-search (arg)
     ;;   "Fontlock search function for dired."
     ;;   (search-forward-regexp
     ;;    (concat "\\(" (format-time-string
     ;;                   "%b %e" (current-time))
     ;;            "\\|"(format-time-string
     ;;                  "%m-%d" (current-time))
     ;;            "\\)"
     ;;            " [0-9]....") arg t))
     ;; (font-lock-add-keywords
     ;;  major-mode
     ;;  (list
     ;;   '(my-dired-today-search . face-file-edited-today)
     ;;   ))

     (defface face-file-edited-today
       '((((class color)
           (background dark))
          (:foreground "GreenYellow"))
         (((class color)
           (background light))
          (:foreground "magenta"))
         (t
          ())) nil)

     (defface face-file-edited-this-week
       '((((class color)
           (background dark))
          (:foreground "LimeGreen"))
         (((class color)
           (background light))
          (:foreground "violet red"))
         (t
          ())) nil)

     (defface face-file-edited-last-week
       '((((class color)
           (background dark))
          (:foreground "saddle brown"))
         (((class color)
           (background light))
          (:foreground "maroon"))
         (t
          ())) nil)

     (defvar face-file-edited-today
       'face-file-edited-today)
     (defvar face-file-edited-this-week
       'face-file-edited-this-week)
     (defvar face-file-edited-last-week
       'face-file-edited-last-week)
     (defun my-dired-today-search (arg)
       "Fontlock search function for dired."
       (search-forward-regexp
        (concat "\\(" (format-time-string "%b %e" (current-time))
                "\\|"(format-time-string "%m-%d" (current-time))
                "\\)"
                " [0-9]....") arg t))

     (defun my-dired-date (time)
       "Fontlock search function for dired."
       (let ((now (current-time))
             (days (* -1 time))
             dateh datel daysec daysh daysl dir
             (offset 0))
         (setq daysec (* -1.0 days 60 60 24))
         (setq daysh (floor (/ daysec 65536.0)))
         (setq daysl (round (- daysec (* daysh 65536.0))))
         (setq dateh (- (nth 0 now) daysh))
         (setq datel (- (nth 1 now) (* offset 3600) daysl))
         (if (< datel 0)
             (progn
               (setq datel (+ datel 65536))
               (setq dateh (- dateh 1))))
         ;;(floor (/ offset 24))))))
         (if (< dateh 0)
             (setq dateh 0))
         ;;(insert (concat (int-to-string dateh) ":"))
         (list dateh datel)))

     (defun my-dired-this-week-search (arg)
       "Fontlock search function for dired."
       (let ((youbi
              (string-to-number
               (format-time-string "%w" (current-time))))
             this-week-start this-week-end day ;;regexp
             (flg nil))
         (setq youbi (+ youbi 1))
         (setq regexp
               (concat "\\("))
         (while (not (= youbi 0))
           (setq regexp
                 (concat
                  regexp
                  (if flg
                      "\\|")
                  (format-time-string
                   "%b %e"
                   (my-dired-date youbi))
                  "\\|"
                  (format-time-string
                   "%m-%d"
                   (my-dired-date youbi))
                  ))
           ;;(insert (concat (int-to-string youbi) "\n"))
           (setq flg t)
           (setq youbi (- youbi 1))))
       (setq regexp
             (concat regexp "\\)"))
       (search-forward-regexp
        (concat regexp " [0-9]....") arg t))

     (defun my-dired-last-week-search (arg)
       "Fontlock search function for dired."
       (let ((youbi
              (string-to-number
               (format-time-string "%w" (current-time))))
             this-week-start this-week-end day ;;regexp
             lyoubi
             (flg nil))
         (setq youbi (+ youbi 0))
         (setq lyoubi (+ youbi 7))
         (setq regexp
               (concat "\\("))
         (while (not (= lyoubi youbi))
           (setq regexp
                 (concat
                  regexp
                  (if flg
                      "\\|")
                  (format-time-string
                   "%b %e"
                   (my-dired-date lyoubi))
                  "\\|"
                  (format-time-string
                   "%m-%d"
                   (my-dired-date lyoubi))
                  ))
           ;;(insert (concat (int-to-string youbi) "\n"))
           (setq flg t)
           (setq lyoubi (- lyoubi 1))))
       (setq regexp
             (concat regexp "\\)"))
       (search-forward-regexp
        (concat regexp " [0-9]....") arg t))

     (font-lock-add-keywords
      major-mode
      (list
       '(my-dired-today-search . face-file-edited-today)
       '(my-dired-this-week-search . face-file-edited-this-week)
       '(my-dired-last-week-search . face-file-edited-last-week)
       ))

     ;; @see http://d.hatena.ne.jp/mooz/20091207/p1
     (defvar my-dired-various-sort-type
       '(("S" . "size")
         ("X" . "extension")
         ("v" . "version")
         ("t" . "date")
         (""  . "name")))

     (defun my-dired-various-sort-change (sort-type-alist &optional prior-pair)
       (when (eq major-mode 'dired-mode)
         (let* (case-fold-search
                get-next
                (options
                 (mapconcat 'car sort-type-alist ""))
                (opt-desc-pair
                 (or prior-pair
                     (catch 'found
                       (dolist (pair sort-type-alist)
                         (when get-next
                           (throw 'found pair))
                         (setq get-next (string-match (car pair) dired-actual-switches)))
                       (car sort-type-alist)))))
           (setq dired-actual-switches
                 (concat "-l" (dired-replace-in-string (concat "[l" options "-]")
                                                       ""
                                                       dired-actual-switches)
                         (car opt-desc-pair)))
           (setq mode-name
                 (concat "Dired by " (cdr opt-desc-pair)))
           (force-mode-line-update)
           (revert-buffer))))

     (defun my-dired-various-sort-change-or-edit (&optional arg)
       "Hehe"
       (interactive "P")
       (when dired-sort-inhibit
         (error "Cannot sort this dired buffer"))
       (if arg
           (dired-sort-other
            (read-string "ls switches (must contain -l): " dired-actual-switches))
         (my-dired-various-sort-change my-dired-various-sort-type)))

     (defvar anything-c-source-dired-various-sort
       '((name . "Dired various sort type")
         (candidates . (lambda ()
                         (mapcar (lambda (x)
                                   (cons (concat (cdr x) " (" (car x) ")") x))
                                 my-dired-various-sort-type)))
         (action . (("Set sort type" . (lambda (candidate)
                                         (my-dired-various-sort-change my-dired-various-sort-type candidate)))))
         ))))

(require 'wdired)
(setq wdired-allow-to-change-permissions t)

;; https://emacs.stackexchange.com/questions/17566/open-file-from-dired-in-the-same-window-using-the-mouse
(defun dired-mouse-find-file (event)
  "In Dired, visit the file or directory name you click on."
  (interactive "e")
  (let (window pos file)
    (save-excursion
      (setq window (posn-window (event-end event))
            pos (posn-point (event-end event)))
      (if (not (windowp window))
          (error "No file chosen"))
      (set-buffer (window-buffer window))
      (goto-char pos)
      (setq file (dired-get-file-for-visit)))
    (if (file-directory-p file)
        (or (and (cdr dired-subdir-alist)
                 (dired-goto-subdir file))
            (progn
              (select-window window)
              (dired-other-window file)))
      (select-window window)
      (find-file (file-name-sans-versions file t)))))

;; wdired keybinds memo
;; | e | wdiredモードになる |
;; | C-c C-c | 編集を完了 |
;; | C-c ESC | 編集を中止する |

(provide '31_init-dired)
;;; 31_init-dired ends here
