;;; early-init.el --- Early initialization -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Andrey Listopadov
;; Keywords: Emacs configuration
;; Homepage: https://gitlab.com/andreyorst/dotfiles.git

;;; Commentary:
;; Emacs 29+ early initialization configuration.

;;; Code:


;;; Straight

(setq package-enable-at-startup nil)


(setq-default load-prefer-newer t)
(setq-default lexical-binding t)
(setq lexical-binding t)

(setq process-adaptive-read-buffering nil)
(setq native-comp-async-report-warnings-errors nil)
(customize-set-value 'gc-cons-threshold most-positive-fixnum)
(setq-default read-process-output-max (* 1024 1024))

;; (setq-default default-frame-alist
;;               (append (list
;;                        '(frame-title-format . nil)
;;                        '(internal-border-width . 5)
;;                        '(tool-bar-lines . 0)
;;                        '(vertical-scroll-bars . nil)
;;                        '(horizontal-scroll-bars . nil)))
;;               initial-frame-alist default-frame-alist
;;               frame-inhibit-implied-resize t
;;               fringe-indicator-alist (assq-delete-all 'truncation fringe-indicator-alist))

(setq byte-compile-warnings nil)

(setq frame-inhibit-implied-resize t)

(setq-default default-frame-alist
              (append (list
                       '(frame-title-format . nil)
                       '(internal-border-width . 2)
                       '(tool-bar-lines . 0)
                       '(vertical-scroll-bars . nil)
                       '(horizontal-scroll-bars . nil))))

(tooltip-mode -1)

(setq-default window-resize-pixelwise t)
(setq-default frame-resize-pixelwise t)

(fringe-mode '(8 . 0))
(setq fringes-outside-margins nil)

;; Don't show icon in frame
(setq-default ns-use-proxy-icon nil)


(provide 'early-init)
;;; early-init.el ends here
