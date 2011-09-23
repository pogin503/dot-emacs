(require 'deferred)
(require 'popup)
(require 'pp)

;; usage: 
;; (global-set-key (kbd "C-x e") 'eval-last-sexp-popup)


(defvar eval-last-sexp-popup-timer   2000) ; timeout msec
(defvar eval-last-sexp-popup-refresh 100)  ; refresh msec
(defvar eval-last-sexp-popup-state   nil)  ; state object
;; (list ; state object
;;    deferred-cancel      ; タイムアウトキャンセル用のdeferred
;;    output-list          ; 現在の表示内容一覧
;;    show-num             ; 前回表示した内容の数（違ってたら再描画）
;;    popup-instance       ; ポップアップ制御
;; )

(defadvice message (around eval-last-sexp-override (txt &rest args))
  (let ((str (apply 'format txt args)))
    (cond
     ((and message-log-max eval-last-sexp-popup-state)
      (push str (cadr eval-last-sexp-popup-state)))
     ((null eval-last-sexp-popup-state)
      (ad-deactivate-regexp "eval-last-sexp-override")))
    ad-do-it))
;; (ad-unadvise 'message)

(defun eval-last-sexp-popup-loop (&optional first)
  (when eval-last-sexp-popup-state
    (destructuring-bind
        (deferred output-list show-num popup)
        eval-last-sexp-popup-state
      (cond
       ((and (null first)
             (not (eq last-command 'eval-last-sexp-popup))) ; カーソール移動した
        ;;消す
        (when (and popup (popup-p popup))
          (popup-delete popup))
        (setq eval-last-sexp-popup-state nil))
       ((= show-num (length output-list)) ; 変化無し
        ;; 何もしない
        )
       (t
        (when (and popup (popup-p popup))
          (popup-delete popup))
        (let ((next-popup
               (popup-tip
                (mapconcat
                 'substring-no-properties
                 (reverse output-list) "\n---------------\n")
                :height 30
                :nowait t)))
          (setq eval-last-sexp-popup-state
                (list deferred output-list
                      (length output-list) 
                      next-popup)))))
      (when eval-last-sexp-popup-state
        (deferred:nextc (deferred:wait eval-last-sexp-popup-refresh)
          (lambda (x) 
            (eval-last-sexp-popup-loop)))))))

(defun eval-last-sexp-popup ()
  (interactive)
  (let* ((cur-state eval-last-sexp-popup-state)
         (val (eval (preceding-sexp)))
         (mstr (deferred:esc-msg (format "%s" val)))
         (str (concat "=> " (pp-to-string val)))
         (popup-state
          (list ; (deferred-cancel output-list show-num popup-instance)
           (deferred:nextc
             (deferred:wait eval-last-sexp-popup-timer)
             (lambda (x) 
               (ad-deactivate-regexp "eval-last-sexp-override")))
           (list str) 0 nil)))
    (when cur-state
      (deferred:cancel (car cur-state)))
    (setq eval-last-sexp-popup-state popup-state)
    (message mstr)
    (ad-activate-regexp "eval-last-sexp-override")
    (eval-last-sexp-popup-loop t)))

(provide 'eval-last-sexp-popup)