;;; 50_init-yasnippet.el --- yasnippet conf
;;; Commentary:
;; @see http://emacs.g.hatena.ne.jp/Shinnya/20100805/1281034504
;; @see http://d.hatena.ne.jp/rubikitch/20101204/yasnippet
;;; Code:

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs
             (concat user-emacs-directory "etc/snippets"))

(yas-global-mode 1)

;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(global-set-key (kbd "H-i") 'yas-expand)
(custom-set-variables
 '(yas-trigger-key "TAB"))

;; | C-x i i | 既存スニペットを挿入する |
;; | C-x i n | 新規スニペットを作成するバッファを用意する |
;; | C-x i v | 既存スニペットを閲覧・編集する |

;; (define-key yas/minor-mode-map (kbd "C-x i f") 'yas/find-snippets)
;; (define-key yas/minor-mode-map (kbd "C-x i e") 'yas/expand)

;;; 50_init-yasnippet.el ends here
