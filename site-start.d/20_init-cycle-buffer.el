;;; 20_init-cycle-buffer --- cycle-buffer conf
;;; Commentary:
;;; Code:
(require '00_init-macro)

(lazyload (cycle-buffer) "cycle-buffer"
		  (req cycle-buffer))

(autoload-if-found 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload-if-found 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
(autoload-if-found 'cycle-buffer-permissive "cycle-buffer" "Cycle forward allowing *buffers*." t)
(autoload-if-found 'cycle-buffer-backward-permissive "cycle-buffer" "Cycle backward allowing *buffers*." t)
(autoload-if-found 'cycle-buffer-toggle-interesting "cycle-buffer" "Toggle if this buffer will be considered." t)

(define-key global-map (kbd "<f9>")    'cycle-buffer-backward)
(define-key global-map (kbd "<f10>")   'cycle-buffer)
(define-key global-map (kbd "S-<f9>")  'cycle-buffer-backward-permissive)
(define-key global-map (kbd "S-<f10>") 'cycle-buffer-permissive)

(provide '20_init-cycle-buffer)
;;; 20_init-cycle-buffer ends here
