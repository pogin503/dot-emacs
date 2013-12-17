;;; 40_init-flymake --- 40_init-flymake
;; This program is free software
;;; Commentary:
;;; Code:

;; Let's run 8 checks at once instead.
;; (setq flymake-max-parallel-syntax-checks 8)
;; (require 'flymake)
;; I don't want no steekin' limits.
;; (setq flymake-max-parallel-syntax-checks nil)

;; Yes, I want my copies in the same dir as the original.
;; (setq flymake-run-in-place t)

;; Nope, I want my copies in the system temp dir.
;; (setq flymake-run-in-place nil)
;; This lets me say where my temp dir is.
;; (setq temporary-file-directory nil)
;; (setq temporary-file-directory nil)
;; (load "~/.emacs.d/plugins/flymake/flymake.elc")
(require 'flymake)

(defun flymake-simple-generic-init (cmd &optional opts)
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))

;; Makefile が無くてもC/C++のチェック
(defun flymake-simple-make-or-generic-init (cmd &optional opts)
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
    (flymake-simple-generic-init cmd opts)))

(defun flymake-c-init ()
  (flymake-simple-make-or-generic-init
   "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

(defun flymake-cc-init ()
  (flymake-simple-make-or-generic-init
   "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))

;; (defun flymake-ruby-init ()
;;   (flymake-simple-make-or-generic-init
;;    "ruby" '("-c")))

;; (defun flymake-ruby-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list "ruby" (list "-c" local-file))))

(add-hook 'ruby-mode-hook '(lambda () (flymake-mode 1)))
;; (require 'flymake-ruby)
;; (add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; flymakeがエラーになっても停止しないようにするための設定
(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
  (setq flymake-check-was-interrupted t))
(ad-activate 'flymake-post-syntax-check)

;; (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
;; (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(push '("\\.[cC]\\'" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.\\(?:cc\|cpp\|CC\|CPP\\)\\'" flymake-cc-init) flymake-allowed-file-name-masks)

(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

;; (add-hook 'c++-mode-hook
;;           '(lambda () (flymake-mode 1)))
;; (add-hook 'c-mode-hook '(lambda () (flymake-mode 1)))
;; (add-hook 'python-mode-hook 'flycheck-mode)
;; (add-hook 'ruby-mode-hook 'flycheck-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)
(provide '40_init-flymake)
;;; 40_init-flymake ends here
