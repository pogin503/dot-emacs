(require 'deferred)
(require 'url-util)

(defvar editable-help-separator "==========")
(defvar editable-help-base-url "http://elisp.net/editable-help/")
(defvar editable-help-category-alist
  '((describe-variable . "variable")
    (describe-function . "function")))

(defvar editable-help-map nil)
(unless editable-help-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'editable-help-next-action)
    (define-key map "\C-c\C-k" 'editable-help-reload)
    (define-key map "\C-m" 'newline)
    (setq editable-help-map map)))

(defun editable-help-reload ()
  (interactive)
  (let ((lang "elisp")
	(category (cdr (assq (car help-xref-stack-item)
			     editable-help-category-alist)))
	(name (and (symbolp (cadr help-xref-stack-item))
		   (symbol-name (cadr help-xref-stack-item)))))
    (when (and lang category name)
      (deferred:$
	(deferred:url-retrieve
	  (concat editable-help-base-url
		  lang "/"
		  category "/"
		  name))
	(deferred:nextc it
	  (lexical-let ((help-buf (current-buffer)))
	    (lambda (buf)
	      (let ((content
		     (with-current-buffer buf
		       (goto-char (point-min))
		       (unless (looking-at "HTTP/[0-9]+.[0-9]+ \\([0-9]+\\)")
			 (error "Invalid status line."))
		       (when (string= (match-string 1) "200")
			 (unless (search-forward "\n\n")
			   (error "Invalid content."))
			 (buffer-substring (match-end 0) (point-max))))))
		(with-current-buffer help-buf
		  (let ((pos-from (editable-help-separator-position))
			(pos-to (or (editable-help-history-position) (point-max)))
			buffer-read-only)
		    (save-excursion
		      (goto-char pos-to)
		      (if pos-from
			  (delete-region pos-from pos-to)
			(insert (propertize (concat editable-help-separator "\n")
					    'editable-help-type 'separator
					    'rear-nonsticky t)))
		      (when content
			(insert (decode-coding-string content 'utf-8) "\n")))))
		(kill-buffer buf)))))
	(deferred:nextc it
	  (lexical-let ((help-buf (current-buffer)))
	    (lambda (buf)
	      (with-current-buffer help-buf
		(editable-help-edit)))))))))

(defun editable-help-make-post-data (data)
  (mapconcat (lambda (x)
	       (let ((key (car x))
		     (val (cdr x)))
		 (when (symbolp key) (setq key (symbol-name key)))
		 (when (consp val) (setq val (car val)))
		 (when (symbolp val) (setq val (symbol-name val)))
		 (concat key "="
			 (url-hexify-string (encode-coding-string val 'utf-8)))))
	     data
	     "&"))

(defun deferred:url-retrieve-post (url data &optional cbargs)
  (lexical-let ((nd (deferred:new))
		(url url)
		(post-data (editable-help-make-post-data data))
		(cbargs cbargs))
    (deferred:next
      (lambda (x)
	(let ((url-request-method "POST")
	      (url-request-data post-data)
	      (url-request-extra-headers
	       '(("Content-Type" . "application/x-www-form-urlencoded"))))
	  (setq buf
		(url-retrieve
		 url (lambda (xx) (deferred:post-task nd 'ok buf))
		 cbargs)))
	nil))
    nd))

(defun editable-help-next-action ()
  (interactive)
  (when (and (buffer-modified-p)
	     (y-or-n-p "Send your change to server? "))
    (editable-help-send-change)))

(define-minor-mode editable-help-mode
  "Editable help."
  :init-value nil
  :lighter "(Editable)"
  :keymap editable-help-map
  (if editable-help-mode
      (editable-help-reload)
    ))

(defun editable-help-history-position ()
  (let ((pos (point-min)))
    (catch 'loop
      (while (setq pos (next-single-property-change pos 'type))
	(when (memq (get-text-property pos 'type) '(help-back help-forward))
	  (throw 'loop pos))))))

(defun editable-help-separator-position ()
  (if (eq (get-text-property (1- (point-max)) 'editable-help-type) 'separator)
      (point-max)
    (let ((pos (point-max)))
      (catch 'loop
	(while (setq pos (previous-single-property-change pos 'editable-help-type))
	  (when (eq (get-text-property (1- pos) 'editable-help-type) 'separator)
	    (throw 'loop pos)))))))

(defun editable-help-edit ()
  (setq buffer-read-only nil)
  (view-mode -1)
  (set-buffer-modified-p nil))

(defun editable-help-send-change ()
  (let* ((pos-from (editable-help-separator-position))
	 (pos-to (or (editable-help-history-position) (point-max))))
    (lexical-let ((str (buffer-substring pos-from pos-to))
		  (lang "elisp")
		  (category (or (cdr (assq (car help-xref-stack-item)
					   editable-help-category-alist))
				(error "No category found.")))
		  (name (symbol-name (cadr help-xref-stack-item)))
		  (help-buf (current-buffer)))
      (deferred:$
	(deferred:url-retrieve-post
	  "http://elisp.net/editable-help/put.php"
	  (list '(lang "elisp")
		(cons 'category category)
		(cons 'name name)
		(cons 'contents str)))
	(deferred:nextc it
	  (lambda (buf)
	    (message "Editable-help: Saved your change for %s/%s/%s"
		     lang category name)
	    (with-current-buffer help-buf
	        (set-buffer-modified-p nil))
	    (kill-buffer buf)))))))
  
(defun editable-help-mode-on ()
  (when (string= (buffer-name) "*Help*")
    (editable-help-mode t)))

(add-hook 'help-mode-hook
	  'editable-help-mode-on)
