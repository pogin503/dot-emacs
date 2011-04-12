;;@see http://d.hatena.ne.jp/YOMOGItaro/20100501/1272674144
;;gauche
(modify-coding-system-alist 'process "gosh" '(utf-8 . utf-8))

(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cs" 'scheme-other-window)

;; ~/elisp をライブラリパスに追加                                                                                                                                                                 
(setq load-path
      (append
       (list
        (expand-file-name "~/elisp/")
        )
       load-path)
)

;;inferior-gauche-mode
(require 'inferior-gauche)
(setq auto-mode-alist
      (cons '("\\.scm$" . inferior-gauche-mode) auto-mode-alist))
(setq default-major-mode 'inferior-gauche-mode)
(inferior-gauche-mode)









