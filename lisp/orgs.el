(defun orgs ()
	(interactive)
	(let ((dir "~/.emacs.d/orgs"))
		(unless (file-directory-p dir)
			(make-directory dir t)))

	(switch-to-buffer (find-file-noselect
										 (format
											"~/.emacs.d/orgs/%s"
											(consult--read
											 (seq-filter (lambda (file)
																		 (string-equal
																			"org"
																			(file-name-extension file)))
																	 (directory-files "~/.emacs.d/orgs")))))))


(global-set-key (kbd "C-c o") 'orgs)

(provide 'orgs)
