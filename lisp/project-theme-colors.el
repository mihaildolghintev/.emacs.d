;;; project-theme-colors --- Consolidated colors for themes -*- lexical-binding: t -*-

;; The options that I'm considering are from the modus color palette:
;;
;; bg-red-intense
;; bg-green-intense
;; bg-yellow-intense
;; bg-blue-intense
;; bg-magenta-intense
;; bg-cyan-intense
;; bg-red-subtle
;; bg-green-subtle
;; bg-yellow-subtle
;; bg-blue-subtle
;; bg-magenta-subtle
;; bg-cyan-subtle
;; bg-red-nuanced
;; bg-green-nuanced
;; bg-yellow-nuanced
;; bg-blue-nuanced
;; bg-magenta-nuanced
;; bg-cyan-nuanced
;; bg-ochre
;; bg-lavender
;; bg-sage

;;; Code:

(require 'modus-themes)
(require 'projectile)
(defvar project/theme-colors/table
  '(("~/code/work/bucket/" . bg-blue-intense)
		("~/code/work/thief/" . bg-yellow-intense)
		("~/.emacs.d/" . bg-green-subtle)
		)
  "A list of projects and their colors.

The `car' of each list item should be of begin with \"~/\" and
 end with \"/\" (so as to conform to multiple machines and
 projectile's interface.")

(cl-defun project/theme-colors/current (&key (default 'bg-blue-subtle))
  "Return a HEX color (e.g. \"#CCDDEE\") for the given project.

The DEFAULT is a named color in the `modus-themes' palette."

  (let* ((project-dir (abbreviate-file-name (or (projectile-project-root) "~/")))
   (name (alist-get project-dir
        project/theme-colors/table
        default nil #'string=)))
   (modus-themes-get-color-value name)))

(defvar project/theme-colors/faces
  (list 'line-number-current-line 'mode-line-active)
  "The faces to update with the theme-colors.")

(defvar project/theme-colors/hooks
  (list 'buffer-list-update-hook
  'projectile-after-switch-project-hook)
  "The hooks to call to set the theme colors.")

(defun project/theme-colors/apply-to-buffer ()
  "Apply the the project's colors to the buffer (e.g. 'mode-line-active)."
  (unless (active-minibuffer-window)
    (dolist (element project/theme-colors/faces)
      (face-remap-add-relative
       element
       `( :background ,(project/theme-colors/current)
    :foreground ,(face-attribute 'default :foreground))))))

(add-hook 'after-init-hook
    (lambda ()
      (dolist (hook project/theme-colors/hooks)
        (add-hook hook #'project/theme-colors/apply-to-buffer))))

(provide 'project-theme-colors)
