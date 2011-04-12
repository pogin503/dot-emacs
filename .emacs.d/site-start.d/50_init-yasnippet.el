;;; Code:

(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")

(require 'yasnippet)
(require 'anything-c-yasnippet)
(setq anything-c-yas-space-match-any-greedy t)
(define-key yas/minor-mode-map (kbd "C-c y") 'anything-c-yas-complete)
(yas/initialize)

;;複数のスニッペットがある場合
(setq yas/root-directory '("~/.emacs.d/etc/mysnippets"
			   "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets"))
(mapc 'yas/load-directory  yas/root-directory)

;;一つしかディレクトリがない場合
;(setq yas/root-directory '"~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")
;(yas/load-directory  yas/root-directory)

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

