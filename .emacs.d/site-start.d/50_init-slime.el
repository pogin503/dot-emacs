;;slime
;(add-to-list 'load-path  "~/.emacs.d/plugins/slime/swank-loader.lisp")
(add-to-list 'load-path "~/plugins/slime/")  ; your SLIME directory
(add-to-list 'load-path "~/plugins/slime/contrib/")  ; your SLIME directory

(add-hook 'lisp-mode-hook (lambda ()
                            (slime-mode t)
                            (show-paren-mode t)))
(add-hook 'inferior-lisp-mode-hook 
	  (lambda () 
	    (inferior-slime-mode t)))

;(custom-set-variables  
; '(slime-backend 
;   (expand-file-name "~/.emacs.d/plugins/slime/swank-loader.lisp")))


;; 文字コードの設定
;(setq slime-lisp-implementations
;      '(
;	;(ccl (,(expand-file-name "~/opt/ccl/scripts/ccl64") "-K"  "utf-8"))
;	(sbcl ("sbcl") :coding-system utf-8-unix)
;	(clisp ("clisp") :coding-system euc-jp-unix)
;	))

(setq slime-net-coding-system 'utf-8-unix)
(setq inferior-lisp-program "sbcl") ; your Lisp system

(req slime)
(slime-setup '(slime-repl slime-fancy slime-banner))
(req slime-autoloads)


;(eval-when (:compile-toplevel :load-toplevel)
;	 (sb-ext:unlock-package 'sb-impl))

;;; start swank server in CL
;(load "swank-loader.lisp" :verbose t)  ; load swank definition files
;(swank:create-server :port 4005)       ; swank connect waiting port 4005

;(setq slime-lisp-implementations
;      `(
;        (ccl (,(expand-file-name "~/opt/ccl/scripts/ccl64") "-K"  "utf-8"))
;        ;; (ccl ("/opt/local/bin/ccl"))
;        (abcl ("/opt/local/bin/abcl"))
;        (clisp ("/opt/local/bin/clisp"))
;        (ecl ("/usr/local/bin/ecl"))
;        (gcl ("/usr/local/bin/gcl"))
;        (sbcl ("/opt/local/bin/sbcl"));
;	))
(set-language-environment "utf-8")

;(slime-setup '(slime-fancy))
;(setq slime-default-lisp 'clisp)


;;slimeのkey-bindのメモ
;'((" "        . slime-space)         ;; 関数名の後等でミニバッファに情報を表示するためのもの．
;  ("\M-p"     . slime-previous-note) ;; 注釈（コンパイル時等のエラーや警告）にジャンプできる．
;  ("\M-n"     . slime-next-note)     ;; 次の注釈にジャンプできる．
;  ("\C-c\M-c" . slime-remove-notes)  ;; 注釈を消す
;  ("\C-c\C-k" . slime-compile-and-load-file) ;; カレントバッファと対応するファイルをコンパイルした後ロードする
;  ("\C-c\M-k" . slime-compile-file)          ;; ファイルをコンパイル
;  ("\C-c\C-c" . slime-compile-defun)         ;; 関数定義をコンパイル
;  ("\C-c\C-l" . slime-load-file)             ;; ファイルをロード
;  ;; Multiple bindings for completion, since M-TAB is often taken by
;  ;; the window manager.
;  ("\M-\C-i"  . slime-complete-symbol)  ;; シンボル補完
;  ("\C-c\C-i" . slime-complete-symbol)  ;; シンボル補完（M-TAB が Window Manger に食われちゃってる場合）
;  ("\M-."     . slime-edit-fdefinition) ;; 関数定義へジャンプ
;  ("\M-,"     . slime-pop-find-definition-stack) ;; M-. の移動を逆にたどる
;  ("\C-x\C-e" . slime-eval-last-expression)      ;; 式を評価する
;  ("\C-c\C-p" . slime-pprint-eval-last-expression) ;; 式を評価して結果を表示
;  ("\M-\C-x"  . slime-eval-defun)                ;; 関数定義を評価する
;  ("\C-c:"    . slime-interactive-eval)          ;; ミニバッファにプロンプトを出し，入力された式を評価
;  ("\C-c\C-z" . slime-switch-to-output-buffer)   ;; slime の出力バッファに移動
;  ("\C-c\C-d" . slime-describe-symbol)           ;; シンボルを describe した結果を表示
;  ("\C-c\M-d" . slime-disassemble-symbol)        ;; シンボルを disassemble した結果を表示
;  ("\C-c\C-t" . slime-toggle-trace-fdefinition)  ;; 関数の TRACE を ON/OFF する
;  ("\C-c\C-a" . slime-apropos)                   ;; apropos
;  ("\C-c\M-a" . slime-apropos-all)               ;; ↑との違いが？？
;  ([(control c) (control m)] . slime-macroexpand-1)    ;; マクロを展開 (macroexpand-1)
;  ([(control c) (meta m)]    . slime-macroexpand-all)  ;; マクロを展開 (macroexpand)
;  ("\C-c\C-g" . slime-interrupt)  ;; SLIME に割り込む
;  ("\C-c\M-g" . slime-quit)       ;; SLIME を終了
;  ("\C-c\M-0" . slime-restore-window-configuration) ;; ？
;  ("\C-c\C-h" . hyperspec-lookup)                   ;; HyperSpec を引く
;  ("\C-c\C-wc" . slime-who-calls)                   ;; ？
;  ("\C-c\C-wr" . slime-who-references)              ;; ？
;  ("\C-c\C-wb" . slime-who-binds)                   ;; ？
;  ("\C-c\C-ws" . slime-who-sets)                    ;; ？
;  ("\C-c\C-wm" . slime-who-macroexpands)            ;; ？
;  ;; Not sure which binding is best yet, so both for now.
;  ([(control meta ?\.)] . slime-next-location)      ;; ？
;  ("\C-c\C- "  . slime-next-location)               ;; ？
;  ("\C-c~"     . slime-sync-package-and-default-directory) ;; パッケージとディレクトリをファイルに合わせる


;;hyperspec
(setq common-lisp-hyperspec-root
      (concat "file://" (expand-file-name "~/.emacs.d/etc/HyperSpec/"))
      common-lisp-hyperspec-symbol-table
      (expand-file-name "~/.emacs.d/etc/HyperSpec/Data/Map_Sym.txt"))

;; HyperSpecをw3mで見る
;(defadvice common-lisp-hyperspec
;  (around hyperspec-lookup-w3m () activate)
;  (let* ((window-configuration (current-window-configuration))
;         (browse-url-browser-function
;          `(lambda (url new-window)
;             (w3m-browse-url url nil)
;             (let ((hs-map (copy-keymap w3m-mode-map)))
;               (define-key hs-map (kbd "q")
;                 (lambda ()
;                   (interactive)
;                   (kill-buffer nil)
;                   (set-window-configuration ,window-configuration)))
;               (use-local-map hs-map)))))
;    ad-do-it))

;;ac-slime
(req ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
