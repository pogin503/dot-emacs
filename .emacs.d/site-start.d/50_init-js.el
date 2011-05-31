(setq js-indent-level 4)
(add-hook-fn 'js-mode-hook
			 (setq c-toggle-auto-newline t)
			 (setq c-toggle-auto-state t)
			 (setq c-toggle-auto-hungry-state 1))