(add-hook 'dired-load-hook (lambda () (load "dired-x")))
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")
            (global-set-key "\C-x\C-j" 'skk-mode)
            ))