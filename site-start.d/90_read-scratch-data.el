;;; auto save and restore scratch buffer
(defun save-scratch-data ()
  (let* ((str (progn
               (set-buffer (get-buffer "*scratch*"))
               (buffer-substring-no-properties
                (point-min) (point-max))))
        (file (expand-file-name (locate-user-emacs-file ".cache/.scratch")))
        (buf (if (get-file-buffer file)
                 (get-file-buffer file)
               (find-file-noselect file))))
    (set-buffer buf)
    (erase-buffer)
    (insert str)
    (save-buffer)
    (kill-buffer buf)))

(defadvice save-buffers-kill-emacs
  (before save-scratch-buffer activate)
  (save-scratch-data))

(defun read-scratch-data ()
  (let ((file (locate-user-emacs-file ".cache/.scratch")))
    (when (file-exists-p file)
      (set-buffer (get-buffer "*scratch*"))
      (erase-buffer)
      (insert-file-contents file))))

(read-scratch-data)    ;; ←これは初期設定ファイルの最後に追加
