(defun hash-exists-p (key table)
  (let ((novalue (make-symbol "<nil>")))
    (not (eq (gethash key table novalue) novalue))))
;; (ex-example 'puthash)
;; (hash-exists-p "apple" __hash_table)
