(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     ;; #!/usr/bin/env runghc 用
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) ;; #!/usr/bin/env runhaskell 用

(lazyload (haskell-mode) "haskell-mode"
          (require 'haskell-mode)
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
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
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
(add-hook 'haskell-mode-hook 'auto-complete-mode)

