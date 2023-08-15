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
    "THIEF SERVER"))

(defun start-service (command name path &optional hook)
  "Run service by COMMAND, NAME and PATH with a HOOK."
  (if (get-buffer name)
      (switch-to-buffer name)
    (setq-local default-directory path)
    (compile
     (format command) t)
    (with-current-buffer "*compilation*"
      (setq-local comint-buffer-maximum-size 500)
      (add-hook 'comint-output-filter-functions #'comint-truncate-buffer)
      (rename-buffer name))
    (when hook
      (add-hook 'kill-emacs-hook hook))))

;; ======================================
;;; BUCKET
;; ======================================

(defun start-bucket-server ()
  "Run bucket rails service."
  (interactive)
  (start-service "bundle exec rails server" "BUCKET SERVER" "~/code/bucket" 'kill-bucket-server))

(defun start-bucket-sidekiq ()
  "Run bucket sidekiq service."
  (interactive)
  (start-service
   "bundle exec sidekiq -C config/sidekiq.yml"
   "BUCKET SIDEKIQ"
   "~/code/bucket"
   (lambda () (kill-sidekiq-instance "bucket"))))

;; ======================================
;;; SAFE
;; ======================================

(defun start-safe-server ()
  "Run safe service."
  (interactive)
  (start-service
   "bundle exec rails server"
   "SAFE SERVER"
   "~/code/safe"
   'kill-safe-server))

(defun start-safe-sidekiq ()
  "Run bucket sidekiq service."
  (interactive)
  (start-service
   "bundle exec sidekiq -C config/sidekiq.yml"
   "SAFE SIDEKIQ"
   "~/code/safe"
   (lambda () (kill-sidekiq-instance "safe"))))

;; ======================================
;;; THIEF
;; ======================================

(defun start-thief-server ()
  "Run safe service."
  (interactive)
  (start-service "bundle exec rails server -p 6000" "THIEF SERVER" "~/code/thief" 'kill-thief-server))

(defun start-aisp ()
  "Run aisp."
  (interactive)
  (start-service "ruby bin/aisp_queue_consumer.rb" "THIEF AISP" "~/code/thief"))

(defun start-pisp ()
  "Run aisp."
  (interactive)
  (start-service "ruby bin/pisp_queue_consumer.rb" "THIEF PISP" "~/code/thief"))

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

;; ======================================
;; KILL
;; ======================================

(defun kill-bucket-server ()
  "Kill bucket server."
  (shell-command "kill -9 $(lsof -ti tcp:5000) 2> /dev/null"))

(defun kill-safe-server ()
  "Kill safe server."
  (shell-command "kill -9 $(lsof -ti tcp:4321) 2> /dev/null"))

(defun kill-thief-server ()
  "Kill safe server."
  (shell-command "kill -9 $(lsof -ti tcp:6000) 2> /dev/null"))

(defun kill-sidekiq-instance (name)
  "Kill bucket sidekiq with NAME."
  (let* ((service-name (string-join (list "sidekiq .* " name)))
         (grep-string (string-join (list "pgrep -f " service-name))))
    (shell-command (string-join (list "kill -9 " grep-string)))))

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

;;; services.el ends here
