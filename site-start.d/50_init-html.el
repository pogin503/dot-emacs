;;; 50_init-html --- 50_init-html
;;; Commentary:
;; @see http://d.hatena.ne.jp/tototoshi/20110127/1296132523
;; web-mode設定
;; @see http://yanmoo.blogspot.jp/2013/06/html5web-mode.html
;;; Code:

(use-package js2-mode
  :config
  (setq js2-strict-trailing-comma-warning nil)
  (add-to-list 'auto-mode-alist '("\\.vue" . js2-mode)))

;; (add-to-load-path "plugins/zencoding")
(use-package web-mode
  :init
  (add-hook 'web-mode-hook 'my-web-mode-conf)
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  ;; インデント数
  (defun my-web-mode-conf ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-indentation nil)
    (custom-set-variables
     '(web-mode-asp-offset 2)
     '(web-mode-code-indent-offset 2)
     '(web-mode-css-indent-offset 2)
     '(web-mode-css-offset 2)
     '(web-mode-enable-auto-expanding t)
     '(web-mode-html-offset 2)
     '(web-mode-java-offset 2)
     '(web-mode-markup-indent-offset 2)
     '(web-mode-php-offset 2)
     '(web-mode-script-offset 2)
     '(web-mode-sql-indent-offset 2))
    ))

;; GET-ing an HTTP page
;;
;; (web-http-get
;;  (lambda (con header data)
;;    (message "the page returned is: %s" data))
;;  :url "http://emacswiki.org/wiki/NicFerrier")

;; POST-ing to an HTTP app
;;
;; (web-http-post
;;  (lambda (con header data)
;;    (message "the data is: %S" data))
;;  :url "http://example.org/postplace/"
;;  :data '(("parameter1" . "data")
;;          ("parameter2" . "more data")))

;; | C-c C-; | コメント/アンコメント                              |
;; | C-c C-e | 閉じていないタグを見つける                         |
;; | C-c C-f | 指定したタグのブロックを開閉する                   |
;; | C-c C-i | 現在開いているバッファをインデントする             |
;; | C-c C-m | マークする(マークする場所によって選択範囲が変わる) |
;; | C-c C-n | 開始・終了タグまでジャンプ                         |
;; | C-c C-r | HTML entitiesをリプレースする                      |
;; | C-c C-s | スニペットを挿入                                   |
;; | C-c C-w | スペースを表示・非表示                             |

;; | C-c /   | 閉じタグを挿入(エレメントを閉じる) |
;; | C-c e b | エレメントの最初へ移動             |
;; | C-c e d | エレメントを削除                   |
;; | C-c e e | エレメントの最後へ移動             |
;; | C-c e e | エレメントを複製                   |
;; | C-c e n | 次のエレメントへ移動               |
;; | C-c e p | 前のエレメントへ移動               |
;; | C-c e u | 親エレメントへ移動                 |
;; | C-c e r | エレメントをリネーム               |
;; | C-c e s | エレメント全体を選択               |
;; | C-c e i | エレメントのコンテンツを選択       |


;; | C-c t b | タグの先頭へ移動(エレメントの先頭では無くタグの先頭です。|
;; |         |  </div>で実行した場合は</div>タグの先頭(<)に移動します)  |
;; | C-c t e | タグの後尾へ移動                                         |
;; | C-c t m | マッチするタグへ移動                                     |
;; | C-c t s | タグを選択                                               |
;; | C-c t p | 前のタグに移動                                           |
;; | C-c t n | 次のタグに移動                                           |

(use-package emmet-mode
  :defines emmet-indentation
  :init
  (defun my-emmet-conf ()
    (setq emmet-indentation 2))
  (add-hook 'emmet-mode-hook 'my-emmet-conf)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'nxml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'php-mode-hook 'emmet-mode)
  :config
  ;; (define-key emmet-mode-keymap (kbd "C-j") nil) ;; C-j は newline のままにしておく
  ;; (keyboard-translate ?\C-i ?\H-i)               ;; C-i と Tabの被りを回避
  (define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-line)
  (define-key emmet-mode-keymap (kbd "H-i") 'emmet-expand-line)
  )

;; @see http://lgfang.github.io/emacs/emacs-xml.html
(use-package hideshow
  :functions (hs-show-block hs-hide-level)
  :config
  (add-hook 'nxml-mode-hook (lambda() (hs-minor-mode 1)))
  (defun lgfang-toggle-level ()
    "mainly to be used in nxml mode"
    (interactive) (hs-show-block) (hs-hide-level 1))
  (use-package nxml-mode
    :config
    (define-key nxml-mode-map (kbd "M-'") 'lgfang-toggle-level)
    (define-key nxml-mode-map [mouse-3] 'lgfang-toggle-level)
    (define-key nxml-mode-map (kbd "<backtab>") 'lgfang-toggle-level)))

;; | M-' or mouse-3 or backtab | 畳み込みのトグル |
;; @memo
;; backtab : Shift + TAB

(defun my-code-golf-for-js ()
   (interactive)
   (mapc (lambda (x)
           (replace-regexp (concat " *\\(" x "\\) *") "\\1" nil (point-min) (point-max)))
           '("-" "=" "\\+" "\\*" "/" "<" ">" "%"))
   (replace-regexp "[ ]*\\([[:digit:]]+\\)[ ]*" "\\1" nil (point-min) (point-max))
   (replace-regexp "for " "for" nil (point-min) (point-max))
   (replace-regexp "var " "" nil (point-min) (point-max))
   (replace-regexp "{ " "{" nil (point-min) (point-max))
   (replace-regexp "^[[:space:]]* " "for" nil (point-min) (point-max)))



(provide '50_init-html)
;;; 50_init-html ends here
