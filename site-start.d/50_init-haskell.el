(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     ;; #!/usr/bin/env runghc 用
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) ;; #!/usr/bin/env runhaskell 用

(add-to-list 'auto-mode-alist '("\\.hamlet$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.lucius$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.julius$" . js2-mode))

(defun my-haskell-ac-init ()
  (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
    (auto-complete-mode t)
    (setq ac-sources '(ac-source-words-in-same-mode-buffers
                       ac-source-dictionary
                       ac-source-ghc-mod))))

(lazyload (haskell-mode literate-haskell-mode haskell-cabal-mode) "haskell-mode"
          (require 'haskell-mode)
          (require 'haskell-cabal)
          (require 'inf-haskell)
          (require 'anything)
          (require 'anything-config)
          (require 'anything-match-plugin)

          (defvar anything-c-source-ghc-mod
            '((name . "ghc-browse-document")
              (init . anything-c-source-ghc-mod)
              (candidates-in-buffer)
              (candidate-number-limit . 9999999)
              (action ("Open" . anything-c-source-ghc-mod-action))))

          (defun anything-c-source-ghc-mod ()
            (unless (executable-find "ghc-mod")
              (error "ghc-mod を利用できません。ターミナルで which したり、*scratch* で exec-path を確認したりしましょう"))
            (let ((buffer (anything-candidate-buffer 'global)))
              (with-current-buffer buffer
                (call-process "ghc-mod" nil t t "list"))))

          (defun anything-c-source-ghc-mod-action (candidate)
            (interactive "P")
            (let* ((pkg (ghc-resolve-package-name candidate)))
              (anything-aif (and pkg candidate)
                  (ghc-display-document pkg it nil)
                (message "No document found"))))

          (defun anything-ghc-browse-document ()
            (interactive)
            (anything anything-c-source-ghc-mod))

          ;; https://github.com/m2ym/auto-complete
          (ac-define-source ghc-mod
            '((depends ghc)
              (candidates . (ghc-select-completion-symbol))
              (symbol . "s")
              (cache)))

          (defun my-ac-haskell-mode ()
            (setq ac-sources '(ac-source-words-in-same-mode-buffers
                               ac-source-dictionary
                               ac-source-ghc-mod)))

          (require 'anything-hasktags)
          ;; M-x anything-ghc-browse-document() に対応するキーの割り当て
          ;; ghc-mod の設定のあとに書いた方がよいかもしれません
          ;; @see http://d.hatena.ne.jp/mizchi/20120426/1335409088
          ;; (require 'flymake)
          ;; (define-key haskell-mode-map (kbd "M-n") 'flymake-goto-next-error)
          ;; (define-key haskell-mode-map (kbd "M-N") 'flymake-goto-prev-error)
          ;; (defun flymake-Haskell-init ()
          ;;   (flymake-simple-make-init-impl
          ;;    'flymake-create-temp-with-folder-structure nil nil
          ;;    (file-name-nondirectory buffer-file-name)
          ;;    'flymake-get-Haskell-cmdline))
          ;; (defun flymake-get-Haskell-cmdline (source base-dir)
          ;;   (list "ghc"
          ;;         (list "--make" "-fbyte-code"
          ;;               (concat "-i"base-dir)
          ;;               source)))
          ;; (defvar multiline-flymake-mode nil)
          ;; (defvar flymake-split-output-multiline nil)
          ;; (defadvice flymake-split-output
          ;;   (around flymake-split-output-multiline activate protect)
          ;;   (if multiline-flymake-mode
          ;;       (let ((flymake-split-output-multiline t))
          ;;         ad-do-it)
          ;;     ad-do-it))
          ;; (defadvice flymake-split-string
          ;;   (before flymake-split-string-multiline activate)
          ;;   (when flymake-split-output-multiline
          ;;     (ad-set-arg 1 "^\\s *$")))
          )

;; (lazyload (haskell-cabal-mode) "haskell-cabal-mode"
;;           (require 'haskell-cabal-mode))

;; (eval-after-load "~/plugins/haskell-mode/haskell-mode.el"
;;   )
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))

;; @see http://d.hatena.ne.jp/jeneshicc/20090309/1236584710
;; (define-key haskell-mode-map "\C-cd" 'flymake-show-and-sit )

;; (defun flymake-show-and-sit ()
;;   "Displays the error/warning for the current line in the minibuffer"
;;   (interactive)
;;   (progn
;;     (let* ( (line-no             (flymake-current-line-no) )
;; 	    (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
;; 	    (count               (length line-err-info-list))
;; 	    )
;;       (while (> count 0)
;; 	(when line-err-info-list
;; 	  (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
;; 		 (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
;; 		 (text (flymake-ler-text (nth (1- count) line-err-info-list)))
;; 		 (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
;; 	    (message "[%s] %s" line text)
;; 	    )
;; 	  )
;; 	(setq count (1- count)))))
;;   (sit-for 60.0)
;;   )
;; ;;

;; (defun define-haskell-mode-conf ()
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.l?hs$" flymake-Haskell-init flymake-simple-java-cleanup))
;;   (add-to-list 'flymake-err-line-patterns
;;                '("^\\(.+\\.l?hs\\):\\([0-9]+\\):\\([0-9]+\\):\\(\\(?:.\\|\\W\\)+\\)"
;;                  1 2 3 4))
;;   (set (make-local-variable 'multiline-flymake-mode) t)
;;   (if (not (null buffer-file-name)) (flymake-mode))
;;   )

;; (add-hook 'haskell-mode-hook 'define-haskell-mode-conf)
(add-to-list 'ac-modes 'haskell-mode)
;; (add-hook 'haskell-mode-hook 'auto-complete-mode)


;; ghc-mod
;; cabal でインストールしたライブラリのコマンドが格納されている bin ディレクトリへのパスを exec-path に追加する

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
(when (or run-linux run-darwin)
  (add-to-list 'exec-path "~/.cabal/bin")
  ;; (add-to-list 'exec-path "~/cabal-dev/bin")
  (if run-darwin (add-to-list 'exec-path "~/Library/Haskell/bin"))
  )

(if run-windows
    (add-to-list 'exec-path (concat "C:/Users/" user-login-name "/AppData/Roaming/cabal/bin")))
;; ghc-flymake.el などがあるディレクトリ ghc-mod を ~/.emacs.d 以下で管理することにした
;; (add-to-list 'load-path "~/.emacs.d/plugins/ghc-mod")

(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook
          (lambda () (ghc-init) (flymake-mode 1)))

(add-hook 'haskell-mode-hook
          (lambda()
            (define-key haskell-mode-map (kbd "C-M-d") 'anything-ghc-browse-document)))

(add-hook 'haskell-mode-hook 'my-ac-haskell-mode)


;; (add-hook 'find-file-hook 'my-haskell-ac-init)

(add-hook 'haskell-mode-hook
          (lambda()
            (define-key haskell-mode-map (kbd "C-c j") 'anything-hasktags-select)))

(load "~/.emacs.d/plugins/haskell-site-file.el")
(setq haskell-program-name "/usr/bin/ghci")
