;;; 50_init-haskell --- 50_init-haskell
;; This program is free software
;;; Commentary:
;;; Code:

;; (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
;; (add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
;; (add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;; #!/usr/bin/env runghc 用
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))
;; #!/usr/bin/env runhaskell 用
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode))

(add-to-list 'auto-mode-alist '("\\.hamlet$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.lucius$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.julius$" . js2-mode))

(require 'mylib)

(require '00_init-macro)
(lazyload (haskell-mode literate-haskell-mode haskell-cabal-mode) "haskell-mode"
          (require 'haskell-mode)
          (require 'haskell-cabal)
          (require 'inf-haskell)

          ;; https://github.com/m2ym/auto-complete
          ;; (ac-define-source ghc-mod
          ;;   '((depends ghc)
          ;;     (candidates . (ghc-select-completion-symbol))
          ;;     (symbol . "s")
          ;;     (cache)))

          ;; (defun my-ac-haskell-mode ()
          ;;   (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary ac-source-ghc-mod)))
          ;; (add-hook 'haskell-mode-hook 'my-ac-haskell-mode)

          ;; (defun my-haskell-ac-init ()
          ;;   (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
          ;;     (auto-complete-mode t)
          ;;     (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary ac-source-ghc-mod))))
          )

;; エコーエリアに関数の型を表示するモードをオンにする
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; haskell-indentationモードを有効にする
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; flycheck-haskellのフック
;; cabal sandboxに対応している
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;; ghc-mod
;; cabal でインストールしたライブラリのコマンドが格納されている bin ディレクトリへのパスを exec-path に追加する

;; (add-to-list 'exec-path
;;              (if run-linux
;;                  (concat (getenv "HOME") "/.cabal/bin")
;;                (if run-windows-x64
;;                    "C:/Program Files (x86)/Haskell Platform/2011.4.0.0/lib/extralibs/bin/")
;;                  "C:/Program Files/Haskell Platform/2011.4.0.0/lib/extralibs/bin/"))
;; (add-to-list 'exec-path
;;              (if run-windows-x64
;;                  "C:/Program Files (x86)/Haskell Platform/2011.4.0.0/bin/"
;;                "C:/Program Files/Haskell Platform/2011.4.0.0/bin/"))

;; path setting
(when (or run-linux run-darwin)
  (add-to-list 'exec-path (expand-file-name "~/.cabal/bin"))
  (when run-darwin
    (add-to-list 'exec-path "~/Library/Haskell/bin")))

(if run-windows
    (add-to-list 'exec-path
                 (concat "C:/Users/" user-login-name "/AppData/Roaming/cabal/bin")))


(defun my-haskell-mode-keybinds ()
  "Set haskell-mode keybindings."
  ;; (define-key haskell-mode-map (kbd "C-M-d") 'anything-ghc-browse-document)
  ;; (define-key haskell-mode-map (kbd "C-c j") 'anything-hasktags-select)
  )

(defun my-haskell-mode-conf ()
  "Set haskell-mode config."
  (ghc-init)
  ;; (setq c-basic-offset 2       ;;基本インデント量4
  ;;       tab-width 4            ;;タブ幅4
  ;;       indent-tabs-mode nil)  ;;インデントをタブでするかスペースでするか
  ;; (make-local-variable 'kill-whole-line)
  (setq kill-whole-line nil)
  (my-ac-haskell-mode)
  (interactive-haskell-mode))

(add-hook 'haskell-mode-hook 'my-haskell-mode-keybinds)
(add-hook 'haskell-mode-hook 'my-haskell-mode-conf)

;; (load "~/.emacs.d/plugins/haskell-site-file.el")
(setq haskell-program-name "/usr/bin/ghci")

;; Haskell align setting
(add-to-list 'align-rules-list
             '(haskell-types
               (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-assignment
               (regexp . "\\(\\s-+\\)=\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-arrows
               (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
             '(haskell-left-arrows
               (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
               (modes quote (haskell-mode literate-haskell-mode))))

;; @see http://karky7.blogspot.jp/2012/12/gentooemacshaskell.html
;; (defadvice inferior-haskell-load-file (after change-focus-after-load)
;;   "Change focus to GHCi window after \\<haskell-mode-map>\\[inferior-haskell-load-file] command."
;;   (other-window 1))
;; (ad-activate 'inferior-haskell-load-file)

(provide '50_init-haskell)
;;; 50_init-haskell ends here
