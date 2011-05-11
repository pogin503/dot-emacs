;;; Code:

(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")

(require 'anything-c-yasnippet)
(setq anything-c-yas-space-match-any-greedy t)
(define-key yas/minor-mode-map (kbd "C-c y") 'anything-c-yas-complete)
(setq anything-c-yas-space-match-any-greedy t) ;スペース区切りで絞り込めるようにする デフォルトは nil

(require 'yasnippet)

;;@see http://emacs.g.hatena.ne.jp/Shinnya/20100805/1281034504
(setq yas/next-field-key "TAB")
(setq yas/prev-field-key "<S-tab>")
(define-key yas/minor-mode-map (kbd "C-x i i") 'yas/insert-snippet)
(define-key yas/minor-mode-map (kbd "C-x i f") 'yas/find-snippets)
(define-key yas/minor-mode-map (kbd "C-x i n") 'yas/new-snippet)
(define-key yas/minor-mode-map (kbd "C-x i v") 'yas/visit-snippet-file)
(define-key yas/minor-mode-map (kbd "C-x i e") 'yas/expand)
;; コメントやリテラルではスニペットを展開しない
(setq yas/buffer-local-condition
      '(or (not (or (string= "font-lock-comment-face"
			     (get-char-property (point) 'face))
		    (string= "font-lock-string-face"
			     (get-char-property (point) 'face))))
             '(require-snippet-condition . force-in-comment)))
(yas/initialize)

;;複数のスニッペットディレクトリがある場合
(setq yas/root-directory '("~/.emacs.d/etc/mysnippets"
			   "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets"))
(mapc 'yas/load-directory  yas/root-directory)

;;一つしかディレクトリがない場合
;(setq yas/root-directory '"~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")
;(yas/load-directory  yas/root-directory)

;;@see http://d.hatena.ne.jp/rubikitch/20101204/yasnippet
;;; [2010/07/13]
(defun yas/expand-link (key)
  "Hyperlink function for yasnippet expansion."
  (delete-region (point-at-bol) (1+ (point-at-eol)))
  (insert key)
  (yas/expand))
;;; [2010/12/02]
(defun yas/expand-link-choice (&rest keys)
  "Hyperlink to select yasnippet template."
  (yas/expand-link (completing-read "Select template: " keys nil t)))
;; (yas/expand-link-choice "defgp" "defcm")

(defun ac-yasnippet-candidate ()
  (let ((table (yas/get-snippet-tables major-mode)))
    (if table
      (let (candidates (list))
            (mapcar (lambda (mode)          
              (maphash (lambda (key value)    
                (push key candidates))          
              (yas/snippet-table-hash mode))) 
            table)
        (all-completions ac-prefix candidates)))))

(defface ac-yasnippet-candidate-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate.")

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white"))) 
  "Face for the yasnippet selected candidate.")

(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (limit . 3)
    (candidate-face . ac-yasnippet-candidate-face)
    (selection-face . ac-yasnippet-selection-face)) 
  "Source for Yasnippet.")
























