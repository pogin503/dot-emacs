;;; 20_init-isearch --- 20_init-isearch
;; This program is free software
;;; Commentary:
;;; Code:
(defvar my-igrep-buffer
  "*Incremental Grep*") ;; grep 結果を表示する window の名前
(defvar my-igrep-window-height
  10) ;; grep 結果を表示する window の高さ
(defvar my-igrep-window-offset
  3) ;; grep 結果中の、現在行の表示位置
(defvar my-igrep-with-color
  t) ;; マッチした文字列に色をつけて表示
(defvar my-igrep-mark-str
  "=>") ;; mark
(defvar my-igrep-min-length
  3) ;; grep する最短文字数
(defvar my-igrep-delay
  0) ;; grep し始めるまでの delay
(defvar my-igrep-light-mode
  nil) ;; migemo （日本語検索）を grep に使うか
(defvar my-igrep-enable-p
  t) ;; igrep オン・オフのの初期設定
(defface my-igface
  '((t (:background "paleturquoise"))) nil) ;; 色
(defvar my-igrep-match 0)
(defvar my-igrep-overlay nil)
(defvar my-igrep-window-configuration nil)

(defadvice isearch-mode
  (before my-igrep activate)
  (when my-igrep-enable-p
    (get-buffer-create my-igrep-buffer)
    (with-current-buffer my-igrep-buffer
      (make-local-variable
       'window-min-height)
      (setq window-min-height 2
            truncate-lines t))))        ; *1

(add-hook
 'isearch-mode-end-hook
 (lambda ()
   (my-igrep-window-cleanup)
   (when my-igrep-enable-p
     (kill-buffer my-igrep-buffer))))

(defadvice isearch-update
  (after my-igrep activate)
  (if (and
       my-igrep-enable-p
       (>= (length isearch-string) my-igrep-min-length))
      (my-igrep-display)
    (my-igrep-window-cleanup)))

(defun my-igrep-toggle-grep-enable ()
  (interactive)
  (message
   (if (setq my-igrep-enable-p
             (not my-igrep-enable-p)) "t" "nil")))

(defun my-igrep-search (&optional end)
  (if (and
       my-igrep-light-mode
       (featurep 'migemo) migemo-isearch-enable-p)
      (re-search-forward
       (migemo-get-pattern isearch-string) end t)
    (search-forward isearch-string end t)))

(defun my-igrep-colorize (str beg end pos)
  (put-text-property
   (- (match-beginning 0) beg) pos 'face 'my-igface str)
  (if (my-igrep-search end)
      (my-igrep-colorize str beg end (- (point) beg)) str))

(defun my-igrep-window-setup () ;; 検索 window の setup
  (set-window-buffer
   (cond
    ((>= (- (window-height)
            my-igrep-window-height) window-min-height)
     (setq my-igrep-window-configuration
           (current-window-configuration))
     (split-window
      (selected-window)           ; 分割可能な window なら分割して表示
      (- (window-height) (1+ my-igrep-window-height))))
    (t (next-window)))
   (get-buffer my-igrep-buffer)))

(defun my-igrep-window-cleanup () ;; 検索 window の cleanup
  (when (window-configuration-p
         my-igrep-window-configuration)
    (set-window-configuration my-igrep-window-configuration)
    (setq my-igrep-window-configuration nil)))

(defun my-igrep-display ()
  (let ((clinen (count-lines (point-min) (point))))
    (when (sit-for my-igrep-delay) ;; (or (= my-igrep-delay 0)
      (save-excursion
        (goto-char (point-min))
        (unless
            (memq
             this-command       ; 検索文字列が変更されたときのみ再検索
             (list 'isearch-repeat-forward
                   'isearch-repeat-backward))
          (setq my-igrep-match 0)
          (with-current-buffer my-igrep-buffer (erase-buffer))
          (let ((linen 1)
                (ppos (point)))
            (while (my-igrep-search)    ; マッチした行だけ処理
              (setq linen
                    (+ linen (1- (count-lines ppos (setq ppos (point))))))
              (let* ((beg
                      (save-excursion
                        (progn (beginning-of-line) (point))))
                     (end
                      (save-excursion
                        (progn (end-of-line) (point))))
                     (str (buffer-substring beg end))) ;-no-properties
                (setq my-igrep-match (1+ my-igrep-match))
                (when my-igrep-with-color
                  (setq str
                        (my-igrep-colorize
                         str beg end (- (point) beg))))
                (with-current-buffer my-igrep-buffer
                  (insert
                   (concat (format "%7d: " linen))
                   str "\n"))))))
        (cond
         ((= my-igrep-match 0) (my-igrep-window-cleanup))
         ((not (window-configuration-p
                my-igrep-window-configuration))
          (my-igrep-window-setup)))
        (when (and
               (> my-igrep-match 0)
               my-igrep-window-configuration)
          (save-selected-window
            (select-window (get-buffer-window my-igrep-buffer))
            (with-current-buffer my-igrep-buffer
              (goto-char (point-max))
              (let* ((alen (length my-igrep-mark-str))
                     (beg (re-search-backward
                           (format "^ +%d:" clinen) nil t))
                     (end (save-excursion (end-of-line) (point)))
                     (alinen (count-lines (point-min) (point))))
                (when (and beg (> my-igrep-match 0))
                  (setq beg (1+ beg))   ; *2
                  (if my-igrep-overlay
                      (move-overlay
                       my-igrep-overlay beg (+ beg alen))
                    (setq my-igrep-overlay
                          (make-overlay beg (+ beg alen))))
                  (overlay-put
                   my-igrep-overlay 'invisible t) ; *3
                  (overlay-put
                   my-igrep-overlay 'before-string my-igrep-mark-str)
                  (when my-igrep-window-configuration ;; リサイズ
                    (enlarge-window
                     (1+
                      (-
                       (if (> my-igrep-match (window-height))
                           (min my-igrep-window-height my-igrep-match)
                         (max window-min-height my-igrep-match))
                       (window-height)))))
                  (when (> my-igrep-match (window-height))
                    (recenter (- my-igrep-window-offset 1)))
                  (setq mode-line-buffer-identification
                        `("%b" ,(format " - %d:%d matches"
                                        alinen my-igrep-match))))
                (force-mode-line-update)))))))))

(provide '20_init-isearch)
;;; 20_init-isearch ends here
