(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (global-set-key "\C-x\C-j" 'skk-mode)
            ))

;;@see http://www.bookshelf.jp/soft/meadow_25.html
(defun dired-my-advertised-find-file ()
  (interactive)
  (let ((kill-target (current-buffer))
        (check-file (dired-get-filename)))
    (funcall 'dired-advertised-find-file)
    (if (file-directory-p check-file)
        (kill-buffer kill-target))))

(defun dired-my-up-directory (&optional other-window)
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

(define-key dired-mode-map "\C-m" 'dired-my-advertised-find-file)
(define-key dired-mode-map "^" 'dired-my-up-directory)
(define-key dired-mode-map [M-up] 'dired-my-up-directory)

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
