(require 'auto-shell-command)

;; Set of key bindings
(global-set-key (kbd "C-c C-m") 'ascmd:toggle) ; Temporarily on/off auto-shell-command run
(global-set-key (kbd "C-c C-,") 'ascmd:popup)  ; Pop up '*Auto Shell Command*'
(global-set-key (kbd "C-c C-.") 'ascmd:exec)   ; Exec-command specify file name

;; Easier to popup on errors (optional, need '(require 'popwin)')
(push '("*Auto Shell Command*" :height 20) popwin:special-display-config)

;; ;; Notification of results to Growl (optional)
;; (defun ascmd:notify (msg) (deferred:process-shell (format "growlnotify -m %s -t emacs" msg))))

;; ;; Register command
;; (ascmd:add '("Target file's regexp" "Command to be executed when the file save"))
;; ;; A simple example
;; (ascmd:add '("/path/to/dir" "ls"))      ; After you have run the `ls` save the following files: '/path/to/dir'
;; ;; High priority S-exp was evaluated after
;; (ascmd:add '("/path/to/dir/foo.c" "ls -la"))      ; 'foo.c' will only run the `ls-la`
;; ;; Special variables
;; (ascmd:add '("/path/to/dir/.*\.c" "cat $FILE"))      ; Other .c file run the `cat FILE_NAME`