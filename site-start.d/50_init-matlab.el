(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))

;; auto-complete-modeの自動起動
(add-to-list 'ac-modes 'matlab-mode)

(setq matlab-shell-command "/usr/local/bin/matlab"  
      matlab-shell-command-swithes '("-nodesktop -v=glnxa64")  
      matlab-indent-level 2  
      matlab-indent-function-body nil  
      matlab-highlight-cross-function-variables t  
      matlab-return-add-semicolon t  
      matlab-show-mlint-warnings t  
      mlint-programs '("~/.emacs.d/plugins/matlab")  
      matlab-mode-install-path (list (expand-file-name "~/matlab/"))  
      )  
(autoload 'mlint-minor-mode "mlint" nil t)  
(add-hook 'matlab-mode-hook (lambda () (mlint-minor-mode 1)))  
(add-hook 'matlab-shell-mode-hook 'ansi-color-for-comint-mode-on)  
(add-hook 'matlab-shell-mode-hook  
	  (lambda () (setenv "LANG" "C")))  
(eval-after-load "shell"  
  '(define-key shell-mode-map [down] 'comint-next-matching-input-from-input))  
(eval-after-load "shell"  
  '(define-key shell-mode-map [up] 'comint-previous-matching-input-from-input)) 