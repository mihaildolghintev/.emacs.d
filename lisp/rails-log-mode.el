;;; rails-log-mode.el --- Major mode for viewing Rails log files

;; Copyright (C) 2012 Anantha Kumaran.

;; Author: Anantha kumaran <ananthakumaran@gmail.com>
;; Version: 0.1
;; Keywords: Rails, log

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ansi-color)
;; (require 'cl)

(defvar rails-log-process nil)
(make-variable-buffer-local 'rails-log-process)


(defvar rails-log-mode-map
  (let ((map (make-sparse-keymap)))
    map))

(defun rails-log-buffer-name (file)
  (let ((project (car (last (split-string (projectile-project-root) "/" t)))))
    (concat "*rails-" project "-" file "-log" "*")))

(define-derived-mode rails-log-mode fundamental-mode "Rails log"
  "Major mode for viewing Rails log files.

\\{rails-log-mode-map}"

  (setq buffer-read-only t)
  (buffer-disable-undo)
  (setq truncate-lines t
        line-move-visual nil))

(defun rails-log-visit-file (button)
  (let ((path (button-get button 'path)))
    (find-file-other-window path)))

(defun rails-log-link-file (start end path)
  (let ((link (make-button start
                           end
                           'help-echo "Visit file"
                           'action #'rails-log-visit-file
                           'follow-link t
                           'mouse-face 'compilation-error-face)))
    (button-put link 'path path)))

(defun rails-log-visit-line (button)
  (let ((path (button-get button 'path))
        (lineno (button-get button 'lineno)))
    (find-file-other-window path)
    (goto-char (point-min))
    (forward-line (1- lineno))))

(defun rails-log-link-line (start end path lineno)
  (let ((link (make-button start
                           end
                           'help-echo "Visit file"
                           'action #'rails-log-visit-line
                           'follow-link t
                           'mouse-face 'compilation-error-face)))
    (button-put link 'path path)
    (button-put link 'lineno lineno)))



(defvar rails-log-regexp-alist

	'(("^ *\\W*\\(.*\\):\\([0-9]+\\):in .*$" .
     (lambda ()
       (let ((lineno (match-string 2))
             (file (match-string 1)))
         (rails-log-link-line (match-beginning 1)
                              (match-end 1)
                              (concat (projectile-project-root) file)
                              (string-to-number lineno)))))


    ;; ("^ *Rendered \\([^ ]+\\.html\\.haml\\) (Duration: [0-9.]+ms \\| Allocations: [0-9]+)" .
    ;;  (lambda ()
    ;;    (rails-log-link-file (match-beginning 1)
    ;;                         (match-end 1)
    ;;                         (concat (projectile-project-root) "app/views/" (match-string 1)))))
		))

(defun rails-log-filter (process output)
  (with-current-buffer (process-buffer process)
    (let ((inhibit-read-only t)
          (start (point-max)))
      (setq output (ansi-color-apply output))
      (goto-char (point-max))
      (insert output)
      (goto-char start)
      (while (not (eobp))

        (beginning-of-line)

        (let ((end (save-excursion (end-of-line) (point)))
              (match-found nil)
              (regexp-alist rails-log-regexp-alist))

          (while (and (car regexp-alist) (not match-found))
            (let ((regex (caar regexp-alist))
                  (callback (cdar regexp-alist)))
              (when (re-search-forward regex end t)
                (funcall callback)
                (setq match-found t))

              (setq regexp-alist (cdr regexp-alist)))))

        (forward-line 1)))))

(defun rails-log-show (file)
  (let ((root (projectile-project-root)))
    (if root
        (let ((log-file (concat root "log/" file ".log"))
              (buffer (get-buffer-create (rails-log-buffer-name file))))
          (setq rails-log-process (start-process "rails-log" buffer "tail" "-n" "1000" "-f" log-file))
          (set-process-filter rails-log-process #'rails-log-filter)
          (switch-to-buffer buffer)
          (rails-log-mode))
      (error "Gemfile not found."))))

;;;###autoload
(defun rails-log-show-development ()
  (interactive)
  (rails-log-show "development"))

;;;###autoload
(defun rails-log-show-test ()
  (interactive)
  (rails-log-show "test"))

;;;###autoload
(defun rails-log-show-production ()
  (interactive)
  (rails-log-show "production"))

(provide 'rails-log-mode)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; rails-log-mode.el ends here
