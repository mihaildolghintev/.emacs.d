;;; services.el --- OBG services

;;; Commentary:

;; Run and manage OBG services

;;; Code:


;; START

(defconst service-list
  '(
    "BUCKET SERVER"
    "BUCKET SIDEKIQ"
    "SAFE SERVER"
    "SAFE SIDEKIQ"
    "THIEF SERVER"
		"ELASTICSEARCH"
		"LOGSTASH"
		"KIBANA"))

(defun get-buffer-name-by-abbr (abbr)
	(pcase abbr
		("BUSE" "BUCKET SERVER")
		("BUSI" "BUCKET SIDEKIQ")
		("SASE" "SAFE SERVER")
		("SASI" "SAFE SIDEKIQ")
		("THSE" "THIEF SERVER")))

(defun kill-all-processes-on-exit ()
  "Kill all processes when quitting Emacs."
  (dolist (proc (process-list))
    (delete-process proc)))

(defun start-service (command name path)
  "Run service by COMMAND, NAME and PATH."
  (save-current-buffer (if (get-buffer name)
       (switch-to-buffer name)
     (setq-local default-directory path)
     (compile
			(format command) t)
     (with-current-buffer "*compilation*"
       (setq-local comint-buffer-maximum-size 500)
       (setq-local truncate-lines t)
			 (font-lock-mode -1)
			 (make-local-variable 'jit-lock-defer-timer)
			 (set (make-local-variable 'jit-lock-defer-time) 0.25)
       (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
       (rename-buffer name)
       (delete-window (get-buffer-window name)))
     (message (format "%s started" name)))))

;; ======================================
;;; BUCKET
;; ======================================

(defun start-bucket-server ()
  "Run bucket rails service."
  (interactive)
  (start-service "bundle exec rails server" "BUCKET SERVER" "~/code/work/bucket"))

(defun start-bucket-sidekiq ()
  "Run bucket sidekiq service."
  (interactive)
  (start-service
   "bundle exec sidekiq -C config/sidekiq.yml"
   "BUCKET SIDEKIQ"
   "~/code/work/bucket"))

;; ======================================
;;; SAFE
;; ======================================

(defun start-safe-server ()
  "Run safe service."
  (interactive)
  (start-service
   "bundle exec rails server"
   "SAFE SERVER"
   "~/code/work/safe"))

(defun start-safe-sidekiq ()
  "Run bucket sidekiq service."
  (interactive)
  (start-service
   "bundle exec sidekiq -C config/sidekiq.yml"
   "SAFE SIDEKIQ"
   "~/code/work/safe"))

;; ======================================
;;; THIEF
;; ======================================

(defun start-thief-server ()
  "Run safe service."
  (interactive)
  (start-service "bundle exec rails server -p 6000" "THIEF SERVER" "~/code/work/thief"))

(defun start-aisp ()
  "Run aisp."
  (interactive)
  (start-service "ruby bin/aisp_queue_consumer.rb" "THIEF AISP" "~/code/work/thief"))

(defun start-pisp ()
  "Run aisp."
  (interactive)
  (start-service "ruby bin/pisp_queue_consumer.rb" "THIEF PISP" "~/code/work/thief"))

;; ======================================
;;; ELK
;; ======================================
(defun start-elastic ()
	"Run elasticsearch."
	(interactive)
	(start-service "bin/elasticsearch" "ELASTICSEARCH" "~/code/elk_local_bucket/elasticsearch-8.7.1"))

(defun start-logstash ()
	"Run logstash."
	(interactive)
	(start-service "bin/logstash -f config/logstash.conf" "LOGSTASH" "~/code/elk_local_bucket/logstash-8.7.1"))

(defun start-kibana ()
	"Run kibana."
	(interactive)
	(start-service "bin/kibana" "KIBANA" "~/code/elk_local_bucket/kibana-8.7.1"))

(defun run-elk ()
	"Run ELK stack."
	(interactive)
	(start-elastic)
	(start-logstash)
	(start-kibana))


;; ======================================
;;; RUN ALL
;; ======================================

(defun run-bucket-stack ()
  "Run all services for bucket."
  (interactive)
  (start-bucket-server)
  (start-bucket-sidekiq)
  (start-safe-server)
  (start-safe-sidekiq)
  (start-thief-server)
  (start-aisp)
  (start-pisp))

(defun services-buffers ()
  "Return only running services buffers."
  (cl-sort (cl-remove-if-not (lambda (buffer)
															 (member (buffer-name buffer) service-list))
														 (buffer-list))
					 (lambda (buffer next-buffer)
						 (string-lessp
							(buffer-name buffer)
							(buffer-name next-buffer)) )))

(defun abbreviate-name (name)
  "Abbreviate NAME by taking the first letter of each word."
  (apply 'concat (mapcar (lambda (word) (substring word 0 2)) (split-string name))))

(defun map-names-to-abbreviations (names)
  "Map a list of NAMES to their abbreviations."
  (mapcar 'abbreviate-name names))

(defun show-abbreviations ()
	(let ((abbrs (mapcar
								(lambda (abbr)
									(if (string-equal (get-buffer-name-by-abbr abbr) (buffer-name (current-buffer)))
											(format " [[[[[%s]]]]] " abbr)
										(format " [%s] " abbr))
									)
								(map-names-to-abbreviations (mapcar #'buffer-name (services-buffers))))))
		(if abbrs
				(string-join abbrs)
			"[No bucket services are running]")))

(defun kill-ruby-instances ()
  "Kill all ruby instances."
  (interactive)
  (async-shell-command "killall -9 rails ruby spring bundle; echo 'Ruby Instances Killed!'" "*Ruby Kill Output*") )

(defun switch-to-buffer-with-message (name)
	(let ((buffer (get-buffer name))
				)
		(if buffer
				(if (string-equal name (buffer-name (current-buffer)))
						(switch-to-buffer (other-buffer))
					(switch-to-buffer buffer)	)
			(message (format "%s is not started" name)))))

(global-set-key
 (kbd "C-c 0")
 (lambda ()
   (interactive)
   (let ((selected-buffer (consult--read
                           (mapcar #'buffer-name (services-buffers))
													 (defun services-buffers ()
														 "Return only running services buffers."
														 (cl-sort (cl-remove-if-not (lambda (buffer)
																													(member (buffer-name buffer) service-list))
																												(buffer-list))
																			(lambda (buffer next-buffer)
																				(string-lessp
																				 (buffer-name buffer)
																				 (buffer-name next-buffer)) )))
                           :prompt "Select running service: "
                           :state (consult--buffer-preview)
                           )))
     (switch-to-buffer selected-buffer))))


(global-set-key (kbd "M-1") (lambda ()
															(interactive)
															(switch-to-buffer-with-message "BUCKET SERVER")))
(global-set-key (kbd "M-2") (lambda ()
															(interactive)
															(switch-to-buffer-with-message "BUCKET SIDEKIQ")))
(global-set-key (kbd "M-3") (lambda ()
															(interactive)
															(switch-to-buffer-with-message "SAFE SERVER")))
(global-set-key (kbd "M-4") (lambda ()
															(interactive)
															(switch-to-buffer-with-message "SAFE SIDEKIQ")))
(global-set-key (kbd "M-5") (lambda ()
															(interactive)
															(switch-to-buffer-with-message "THIEF SERVER")))


(provide 'services)
