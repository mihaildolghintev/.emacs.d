
(defun current-buffers (mode)
	(seq-filter
	 (lambda (item) (string-match-p mode item))
	 (mapcar 'buffer-name (buffer-list))))

(defun extract-max-number (lst)
  (if (null lst)
      1
    (let ((numbers (mapcar (lambda (item)
                             (if (string-match "\\([0-9]+\\)" item)
                                 (+ 1 (string-to-number (match-string 1 item)))
                               nil))
                           lst)))
      (or (apply 'max (delq nil numbers)) 1))))

(defun create-scratch-buffer ()
	(interactive)
	(let* ((modes '("ruby" "json" "org" "markdown" "emacs-lisp"))
				 (mode (consult--read modes))
				 (buffer (generate-new-buffer (format "*scratch %s #%i*" mode (extract-max-number (current-buffers mode))))))
		(switch-to-buffer buffer)
		(eval (car (read-from-string (format "(%s-mode)" mode))))))

(provide 'scratch-buffers)
