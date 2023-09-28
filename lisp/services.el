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

(defun kill-all-processes-on-exit ()
  "Kill all processes when quitting Emacs."
  (dolist (proc (process-list))
    (delete-process proc)))

(defun start-service (command name path)
  "Run service by COMMAND, NAME and PATH."
  (if (get-buffer name)
      (switch-to-buffer name)
    (setq-local default-directory path)
    (compile
     (format command) t)
    (with-current-buffer "*compilation*"
      (setq-local comint-buffer-maximum-size 500)
      (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
      (rename-buffer name)
      (delete-window (get-buffer-window name)))
    (message (format "%s started" name))))

;; ======================================
;;; BUCKET
;; ======================================

(defun start-bucket-server ()
  "Run bucket rails service."
  (interactive)
  (start-service "bundle exec rails server" "BUCKET SERVER" "~/code/bucket"))

(defun start-bucket-sidekiq ()
  "Run bucket sidekiq service."
  (interactive)
  (start-service
   "bundle exec sidekiq -C config/sidekiq.yml"
   "BUCKET SIDEKIQ"
   "~/code/bucket"))

;; ======================================
;;; SAFE
;; ======================================

(defun start-safe-server ()
  "Run safe service."
  (interactive)
  (start-service
   "bundle exec rails server"
   "SAFE SERVER"
   "~/code/safe"))

(defun start-safe-sidekiq ()
  "Run bucket sidekiq service."
  (interactive)
  (start-service
   "bundle exec sidekiq -C config/sidekiq.yml"
   "SAFE SIDEKIQ"
   "~/code/safe"))

;; ======================================
;;; THIEF
;; ======================================

(defun start-thief-server ()
  "Run safe service."
  (interactive)
  (start-service "bundle exec rails server -p 6000" "THIEF SERVER" "~/code/thief"))

(defun start-aisp ()
  "Run aisp."
  (interactive)
  (start-service "ruby bin/aisp_queue_consumer.rb" "THIEF AISP" "~/code/thief"))

(defun start-pisp ()
  "Run aisp."
  (interactive)
  (start-service "ruby bin/pisp_queue_consumer.rb" "THIEF PISP" "~/code/thief"))

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
  (cl-remove-if-not (lambda (buffer)
                      (member (buffer-name buffer) service-list))
                    (buffer-list)))

(defun kill-ruby-instances ()
  "Kill all ruby instances."
  (interactive)
  (async-shell-command "killall -9 rails ruby spring bundle; echo 'Ruby Instances Killed!'" "*Ruby Kill Output*") )

(global-set-key
 (kbd "C-c 0")
 (lambda ()
   (interactive)
   (let ((selected-buffer (consult--read
                           (mapcar #'buffer-name (services-buffers))
                           :prompt "Select running service: "
                           :state (consult--buffer-preview)
                           )))
     (switch-to-buffer selected-buffer))
   ))


(defun rails/remote-console ()
  "Start a remote console"
  (interactive)
  (compile
   (format "~/code/bucket/bin/rails server") t)
  (rename-buffer "BUCKET SERVER"))

(provide 'services)
