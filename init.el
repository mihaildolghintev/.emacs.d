(defvar straight-process-buffer)
(setq-default straight-process-buffer " *straight-process*")

(defvar straight-build-dir)
(setq straight-build-dir (format "build-%s" emacs-version))

(defvar straight-repository-branch)
(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
       (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(defvar-local compilation--start-time nil
  "The time when the compilation started as returned by `float-time'.")

(eval-after-load "bytecomp"
  '(add-to-list 'byte-compile-not-obsolete-vars
     'font-lock-beginning-of-syntax-function))

(use-package use-package
  :straight (:type built-in)
  :custom
  (use-package-enable-imenu-support t))

(use-package cus-edit
  :straight (:type built-in)
  :config
  (setq custom-file "~/.emacs.d/custom.el")
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))
  (load custom-file))

(use-package benchmark-init
  :straight t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package esup
  :straight t)

(setq esup-depth 0)

(use-package explain-pause-mode
  :straight (:host github
							:repo "lastquestion/explain-pause-mode")
  :config
  (setq explain-pause-alert-style 'silent))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package org
  :straight t
	:defer t
  :config
  (setq org-babel-load-languages '((emacs-lisp . t)
                                    (ruby . t)
																	  (sql . t))))

(use-package blamer
  :straight t
  :bind (("C-c i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 1)
  (blamer-min-offset 70)
  (blamer-max-commit-message-length 100)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                  :height 100
                  :italic t))))

(use-package expand-region
  :straight t
  :defer t
  :bind ("C-=" . er/expand-region)
  :config
  (evil-set-command-property 'er/expand-region :jump t))

(use-package delsel
  :straight (:type built-in)
  :defer 1
  :config (delete-selection-mode 1)
  )

(use-package ns-auto-titlebar
  :straight t
	:disabled t
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))

(use-package autorevert
  :straight (:type built-in)
  :hook ((message-mode . auto-revert-tail-mode)
          (display-warning . auto-revert-tail-mode))
  :config
  (setq global-auto-revert-non-file-buffers t
    auto-revert-check-vc-info t
		auto-revert-interval 1
		auto-revert-use-notify nil
    auto-revert-verbose nil)
  (global-auto-revert-mode t))

(use-package mule-util
  :straight nil
  :config
  (setq truncate-string-ellipsis "…"))

(use-package compile
  :straight (:type built-in)
  :config
  (setq compilation-scroll-output t))

(use-package ultra-scroll-mac
	:straight (:host github :repo "jdtsmith/ultra-scroll-mac")
	:if (eq window-system 'mac)
	:init
  (setq scroll-conservatively 101 ; important!
    scroll-margin 0)
  :config
  (ultra-scroll-mac-mode 1))

(use-package comp
	:straight (:type built-in)
	:if (native-comp-available-p)
	:config
	(setq native-comp-speed 2))

(use-package emacs
  :straight nil
  :config
  (setq-default
    fill-column 100
    sentence-end-double-space nil
    redisplay-skip-fontification-on-input t
    frame-title-format '(""
                          "%b"
                          (:eval
                            (let ((project-name (projectile-project-name)))
                              (unless (string= "-" project-name)
                                (format (if (buffer-modified-p) "   ◉   [ %s ]" "    ●    [ %s ]") (upcase project-name))))))
    highlight-nonselected-windows nil
    x-underline-at-descent-line t
    history-delete-duplicates t
    history-length t
    kill-buffer-delete-auto-save-files t
    truncate-lines t
    bidi-paragraph-direction 'left-to-right
    cursor-in-non-selected-windows nil
    inhibit-compacting-font-caches t))
(setq
  message-log-max 16384
  load-prefer-newer t
  use-file-dialog nil
  use-dialog-box nil)

(use-package terminal
  :straight (:type built-in)
  :config
  (setq ring-bell-function 'ignore))

(use-package gcmh
	:straight t
	:init
	(setq gcmh-idle-delay 5
		gcmh-high-cons-threshold (* 16 1024 1024))
	:config

	(gcmh-mode))

(use-package uniquify
  :straight nil
  :defer 3
  :config
  (setq uniquify-buffer-name-style 'reverse
    uniquify-separator " • "
    uniquify-after-kill-buffer-p t
    uniquify-ignore-buffers-re "^\\*"))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package window
  :straight nil
  :config
  (setq split-height-threshold 99999999999999999
		auto-window-vscroll nil))

(use-package message
  :straight nil
  :config
  (setq message-kill-buffer-on-exit t))

(use-package ns-win
	:straight (:type built-in)
	:if (eq system-type 'darwin)
	:config
	(setq mac-command-modifier 'meta
		mac-option-modifier nil
		mac-control-modifier 'control))

(use-package server
	:straight (:type built-in)
  :config
  (unless (server-running-p)
    (server-start)))

(use-package frame
	:straight (:type built-in)
	:config
	(setq window-divider-default-right-width 8
		window-divider-default-places 'right-only
		mouse-highlight t)
	(window-divider-mode 1))

(use-package subword
	:straight (:type built-in)
	:config
	(global-subword-mode 1))

(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")
(modify-coding-system-alist 'process "*" 'utf-8)


(global-set-key (kbd "M-o") 'crux-other-window-or-switch-buffer)

(use-package fontaine
  :straight t
  :config
  (setq fontaine-presets
		'((compact
				:default-height 105)
			 (default
				 :default-height 130)
			 (t
				 :default-family "JetBrains Mono"
				 :default-weight regular
				 :default-height 105
				 :fixed-pitch-family nil ; falls back to :default-family
				 :fixed-pitch-weight nil ; falls back to :default-weight
				 :fixed-pitch-height 1.0
				 :fixed-pitch-serif-family nil ; falls back to :default-family
				 :fixed-pitch-serif-weight nil ; falls back to :default-weight
				 :fixed-pitch-serif-height 1.0
				 :variable-pitch-family "ETBembo"
				 :variable-pitch-weight nil
				 :variable-pitch-height 1.18
				 :bold-family nil ; use whatever the underlying face has
				 :bold-weight medium
				 :italic-family nil
				 :italic-slant italic
				 :line-spacing nil)))
  (fontaine-set-preset 'default))

(use-package dired
  :straight nil
  :config
  (setq dired-dwim-target t
    dired-recursive-copies t
		dired-recursive-deletes t
		dired-listing-switches "-laGhpX"
    dired-auto-revert-buffer t
		dired-use-ls-dired t
    dired-kill-when-opening-new-dired-buffer t))

(use-package dired-x
  :straight (:type built-in)
  :after dired)

(use-package display-line-numbers
  :straight t
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :config
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode))

;; (use-package mood-line
;;   :straight t
;;   :hook ((after-init . mood-line-mode))
;; 	:init
;; 	(setq display-time-default-load-average nil
;; 		display-time-format "%I:%M %p %e %b"))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 25
    doom-modeline-bar-width 4
    doom-modeline-project-detection 'projectile
    doom-modeline-buffer-file-name-style 'relative-from-project
    doom-modeline-buffer-encoding nil
    doom-modeline-total-line-number t
    doom-modeline-vcs-max-length 50
    doom-modeline-env-enable-ruby nil
    ))

(use-package avy
  :straight t
  :config
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

(use-package battery
	:straight (:type built-in)
	:config
	(display-battery-mode 1))

(use-package indent-guide
  :straight t
	:hook (((ruby-mode ruby-ts-mode coffee-mode haml-mode web-mode yaml-mode) . indent-guide-mode)))

(use-package hungry-delete
  :straight t
  :config
  (setq hungry-delete-except-modes '(coffee-mode haml-mode web-mode sql-mode))
  (global-hungry-delete-mode t))

(use-package startup
  :no-require
  :config
	(setq inhibit-splash-screen t
		user-mail-address "mihail.dolghintev@saltedge.com"
		user-full-name "Mihail Dolghintev"
		initial-scratch-message nil
		initial-major-mode 'emacs-lisp-mode
		site-run-file nil))

(use-package files
  :preface
  (defvar auto-save-dir
    (expand-file-name ".cache/auto-save/" user-emacs-directory)
    "Directory to store auto-save files.")
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (auto-save-file-name-transforms
    `((".*" ,auto-save-dir t)))
  (auto-save-no-message t)
  (auto-save-interval 100)
  (require-final-newline t)
  :config
  (setq delete-old-versions t
		insert-directory-program "gls"
		confirm-kill-emacs 'y-or-n-p)
  (setq-default backup-directory-alist
		'(("." . "~/.cache/emacs/auto-save"))
		backup-by-copying t
		delete-old-versions t)
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

(use-package diff-mode
  :straight nil
  :config
  (setq-default diff-add-log-use-relative-names t))

(use-package subr
  :no-require
	:defer t
  :init
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package super-save
  :straight t
  :config
  (super-save-mode +1))

;; (use-package recentf
;; 	:config
;; 	(recentf-mode 1)
;; 	(setq recentf-max-saved-items 50))


(use-package saveplace
  :init
  (save-place-mode t))

;; (use-package company
;;   :straight t
;;   :config
;;   (add-hook 'prog-mode-hook #'company-mode)
;;   (add-hook 'web-mode-hook #'company-mode)
;;   (add-hook 'magit-mode-hook #'company-mode)
;;   (setq company-format-margin-function 'company-dot-icons-margin
;;     company-dot-icons-format            " ● "
;;     company-minimum-prefix-length 2
;;     company-tooltip-limit 8
;;     company-tooltip-minimum-width 40
;;     company-tooltip-margin 2
;;     company-tooltip-offset-display 'lines
;;     company-idle-delay 0
;;     company-echo-delay 0
;;     company-dabbrev-ignore-case nil
;;     company-dabbrev-other-buffers nil
;;     )

;;   (global-set-key (kbd "M-i") 'consult-company)
;;   (define-key company-active-map (kbd "M-n") nil)
;;   (define-key company-active-map (kbd "M-p") nil)
;;   (define-key company-active-map (kbd "C-n") #'company-select-next)
;;   (define-key company-active-map (kbd "C-p") #'company-select-previous))


;; (use-package company-box
;;   :straight t
;; 	:after company
;;   :hook (company-mode . company-box-mode)
;;   :config
;;   (setq company-box-enable-icon nil
;;         company-box-scrollbar nil
;;         company-box-minimum-width 40
;;         company-box-doc-enable nil))

(use-package eldoc
  :straight nil
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package simple
  :bind (("M-z" . zap-up-to-char)
          ("M-S-z" . zap-to-char)
          ("C-x k" . kill-current-buffer)
          ("C-h C-f" . describe-face))
  :hook ((before-save . delete-trailing-whitespace))
  :config
  (setq indent-tabs-mode nil
    yank-excluded-properties t
    blink-matching-delay 0
    blink-matching-paren nil
    copy-region-blink-delay 0
    eval-expression-print-level 100
    shell-command-default-error-buffer "*Shell Command Errors*"))

(use-package bookmark
  :straight nil
  :bind (("C-c b m" . bookmark-set)
          ("C-c b b" . consult-bookmark)
          ("C-c b d" . bookmark-delete)))

(use-package vertico
  :straight t
  :config
  (setq enable-recursive-minibuffers t
    completion-ignore-case t
    vertico-count 17
    vertico-resize nil
    read-buffer-completion-ignore-case t)
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
          ("RET" . vertico-directory-enter))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

(use-package orderless
  :straight t
  :config
  (setq completion-styles '(orderless basic)
    completion-category-defaults nil
    completion-category-overrides '((file (styles . (partial-completion))))))

(use-package modus-themes
  :straight t
  :config
  (setq modus-themes-italic-constructs t
    modus-themes-bold-constructs nil)
  (setq modus-themes-common-palette-overrides
		'((bg-mode-line-active bg-yellow-subtle)
			 (fg-mode-line-active fg-main)

			 (border-mode-line-active yellow-intense)
       (cursor red)
       )))


(load-theme 'modus-operandi t)
(set-face-attribute 'font-lock-comment-face nil :background "lemon chiffon")

(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
	:bind (("C-c l" . consult-git-log-grep)
				  ("C-c n" . create-scratch-buffer)
          ("C-c g" . consult-ripgrep)
          ("M-i" . completion-at-point))
  :init
  (setq register-preview-delay 0
    register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
    consult-theme :preview-key '(:debounce 0.2 any)
    consult-ripgrep consult-git-grep consult-grep
    consult-bookmark consult-recent-file consult-xref
    consult--source-bookmark consult--source-file-register
    consult--source-recent-file consult--source-project-recent-file
    :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  (setq consult-locate-args "mdfind -name")
  (setq consult-async-min-input 2)
  (autoload 'projectile-project-root "projectile")
  (setq completion-in-region-function
    (lambda (&rest args)
      (apply (if vertico-mode
               #'consult-completion-in-region
               #'completion--in-region)
        args)))
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-flycheck
  :straight t
  :after flycheck)

(use-package embark
  :straight t
  :bind (("C-." . embark-collect)
				  ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :hook
	(embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-git-log-grep
  :straight t
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

(use-package multiple-cursors
  :straight t
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
	(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
	(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package devdocs
  :straight t
  :config
  (global-set-key (kbd "C-h d") 'devdocs-lookup))

(use-package deadgrep
  :straight t
  :config
	(setq deadgrep-extra-arguments '("--follow" "--no-config"))
  (global-set-key (kbd "C-c f") #'deadgrep))

(use-package symbol-overlay
  :straight t
  :config
  (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
  (global-set-key (kbd "M-N") 'symbol-overlay-jump-prev))

(use-package projectile
  :straight t
	:defer t
  :bind (:map projectile-mode-map
          ("C-c p" . projectile-command-map))
  :init
  (projectile-mode 1)
  :config
  (setq projectile-project-search-path '("~/code/work")
    projectile-project-root-files-functions '(projectile-root-local
                                               projectile-root-bottom-up)))

(use-package projectile-rails
  :straight t
  :after projectile
  :config
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)
  (projectile-rails-global-mode))

(use-package paren
  :hook (after-init . show-paren-mode)
  :init
  (setq blink-matching-paren nil
		show-paren-delay 0.0
    show-paren-style 'parenthesis
    show-paren-highlight-openparen t
		show-paren-priority 1
		show-paren-when-point-in-periphery t
		show-paren-when-point-inside-paren t)
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold))

(use-package css-mode
  :straight t
  :custom
  (css-indent-offset 2))

(use-package markdown-mode
  :straight t
  :mode
  ("README\\.md\\'" . gfm-mode)
  ("\\.md\\'" . markdown-mode)
  ("\\.markdown\\'" . markdown-mode)
	:config
	(setq markdown-make-gfm-checkboxes-buttons t
		markdown-gfm-uppercase-checkbox t
    markdown-fontify-code-blocks-natively t))

(use-package json-mode
  :straight t)

(use-package csv-mode
  :straight t
  :defer t
  :custom
  (csv-align-max-width 80))

(use-package puni
  :straight t)

(use-package elfeed
  :straight t
	:defer t
  :config
  (setq elfeed-search-title-max-width 140)
  (setq elfeed-feeds
		'("http://nullprogram.com/feed/"
			 "https://planet.emacslife.com/atom.xml"
			 "https://allaboutcoding.ghinda.com/rss.xml"
			 "https://arialdomartini.github.io/feed.xml"
			 "https://rubypilot.com/"
       "https://karthinks.com/index.xml"
       "https://hackerstations.com/index.xml"
       "https://www.masteringemacs.org/feed/"
       "https://tony-zorman.com/atom.xml"
       "http://nullprogram.com/feed/"
       "https://batsov.com/atom.xml"
       "https://www.reddit.com/r/emacs.rss"
       "https://www.crunchydata.com/blog/rss.xml"
       "https://metaredux.com/feed.xml"
       "https://lisp-journey.gitlab.io/index.xml"
			 "https://melpa.org/updates.rss")))

(use-package popper
	:straight t
  :bind (("C-`"   . popper-toggle)
          ("M-`"   . popper-cycle)
          ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
    '("\\*Messages\\*"
       "Output\\*$"
       "\\*Async Shell Command\\*"
			 "^\\*vterm.*\\*$"  vterm-mode
       inf-ruby-mode
       help-mode
			 helpful-mode
       compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package avy
  :straight t)

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t
    evil-want-keybinding nil
    evil-split-window-below t
    evil-vsplit-window-right t
    evil-want-C-d-scroll nil
    evil-move-beyond-eol t
    evil-want-C-u-scroll nil
    evil-shift-width 2
    evil-want-fine-undo t
    evil-highlight-closing-paren-at-point-states '(not emacs insert replace normal)
    evil-disable-insert-state-bindings t)

  :config
	(define-key evil-motion-state-map "(" nil)
	(define-key evil-motion-state-map ")" nil)
	(define-key evil-visual-state-map "(" nil)
	(define-key evil-replace-state-map "(" nil)
  (evil-mode t)
  (setq evil-search-module 'evil-search
    evil-undo-system 'undo-fu)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'help-mode 'normal)
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'compilation-mode 'normal)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "TAB") 'indent-for-tab-command)
  (evil-define-key 'visual 'global (kbd "TAB") 'indent-for-tab-command)
  (evil-define-key 'normal 'global (kbd "<leader>e") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "M-r") 'consult-imenu)
  (evil-define-key 'normal 'global (kbd "M-p") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "C-M-p") 'consult-projectile-switch-project)
  (evil-define-key 'normal 'global (kbd "M-f") 'consult-line)
  (evil-define-key 'normal 'global (kbd "C-e") 'end-of-line)
  (evil-define-key 'visual 'global (kbd "C-e") 'end-of-line)
  (evil-define-key 'insert 'global (kbd "C-e") 'end-of-line)

	(evil-define-key 'normal 'global (kbd "C-f") 'forward-char)
	(evil-define-key 'normal 'global (kbd "C-b") 'backward-char)


  (evil-define-key 'normal 'global (kbd "C-p") 'previous-line)
  (evil-define-key 'normal 'global (kbd "C-n") 'next-line)

  (evil-define-key 'normal 'global (kbd "M-w") 'yad/delete-window)

  (evil-define-key 'visual 'global (kbd "M-[") 'puni-wrap-square)
  (evil-define-key 'visual 'global (kbd "{") 'puni-wrap-curly)
  (evil-define-key 'visual 'global (kbd "(") 'puni-wrap-round)

  (evil-define-key 'visual 'global (kbd "K") 'drag-stuff-up)
  (evil-define-key 'visual 'global (kbd "J") 'drag-stuff-down)

  (evil-define-key 'visual 'global (kbd "M-(") 'puni-wrap-round)
  (evil-define-key 'visual 'global (kbd "M-[") 'puni-wrap-square)
  (evil-define-key 'visual 'global (kbd "M-{") 'puni-wrap-curly)

  (evil-define-key 'normal 'global (kbd "<leader>ka") 'paredit-splice-sexp-killing-backward)
  (evil-define-key 'normal 'evil-matchit-mode-map "m" 'evilmi-jump-items)
  )

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init))

(use-package evil-org
	:straight t
	:hook (org-mode . evil-org-mode))

(use-package evil-matchit
  :straight t
  :after evil
  :config
  (evilmi-load-plugin-rules '(ruby-base-mode ruby-ts-mode) '(simple ruby))

  (global-evil-matchit-mode 1))

(use-package undo-fu
  :straight t
  :config
  (setq undo-limit 400000
    undo-strong-limit 3000000
    undo-outer-limit 48000000)
  :bind (([remap undo] . undo-fu-only-undo)
          ([remap redo] . undo-fu-only-redo)))

(use-package vundo
  :straight t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
    vundo-compact-display t)
  :bind (("C-x u" . vundo)))

;; (use-package smartparens
;;   :straight t
;;   :hook ((prog-mode text-mode markdown-mode) . smartparens-mode)
;;   :config
;;   (require 'smartparens-config)
;;   (sp-with-modes 'ruby-mode
;;     (sp-local-pair "def" nil :actions nil)
;;     (sp-local-pair "do" nil :actions nil)
;;     (sp-local-pair "if" nil :actions nil)
;;     (sp-local-pair "case" nil :actions nil))
;;   (sp-with-modes 'emacs-lisp-mode
;;     (sp-local-pair "'" nil :actions nil)
;;     (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p)))
;;   )

(use-package electric
  :straight (:type built-in)
  :config
  (setq electric-indent-mode t))


(use-package elisp-mode
  :custom
  (eval-expression-print-level nil)
  (eval-expression-print-length nil)
  :bind
  (:map emacs-lisp-mode-map
    ("C-c C-e" . eval-last-sexp)
    ("C-c C-c" . eval-defun)
    ("C-c C-b" . eval-buffer)
    ("C-c C-r" . eval-region)))


(use-package vc-hooks
  :config
  (setq vc-follow-symlinks t))

(use-package magit
  :straight t
  :bind (("C-c m m" . magit-status)
          ("C-c m d" . magit-dispatch)
          ("C-c m f" . magit-file-dispatch))
  :custom
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk t)
  (magit-revert-buffers 'silent)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-section-initial-visibility-alist '((untracked . show)
                                             (unstaged . show)
                                             (unpushed .show)
                                             (unpulled . show)
                                             (stashes . show)))
  :config
  (magit-auto-revert-mode)
   (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
	(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
	(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq magit-no-confirm '(stage-all-changes unstage-all-changes)
		auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p))

(use-package diff-hl
	:straight t
  :defer 2
	:config
	(global-diff-hl-mode))

(use-package elisp-def
	:straight t
  :commands (elisp-def elisp-def-mode)
	:hook ((emacs-lisp-mode . elisp-def-mode)
				  (ielm-mode . elisp-def-mode)
				  (lisp-interaction-mode . elisp-def-mode)))

(use-package eros
  :straight t
  :config
  (eros-mode 1))

(use-package eval-expr
  :straight t)

(use-package rg
  :straight t)

(use-package pulsar
  :straight t
  :config
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-yellow)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

(use-package helpful
  :straight t
	:defer t
  :bind (([remap describe-function] . helpful-callable)
          ([remap describe-command]  . helpful-command)
          ([remap describe-variable] . helpful-variable)
          ([remap describe-key]      . helpful-key)
          ([remap describe-symbol]   . helpful-symbol)))

(use-package elisp-demos
	:straight t
	:config
	(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package crux
  :straight t)

(use-package which-key
  :straight t
  :defer 2
  :config
  (which-key-mode)
  (setq which-key-max-display-columns 2
    which-key-add-column-padding 10
    which-key-max-description-length 120))

(use-package uniquify
	:straight nil
  :init
  (setq uniquify-buffer-name-style 'forward
    uniquify-min-dir-content 2))

(use-package lin
  :straight t
  :config
  (add-hook 'dired-sidebar-mode-hook 'lin-mode))

(use-package tree-sitter
  :straight t
	:defer t
  :config
  (global-tree-sitter-mode))

;; (use-package treesit-auto
;;   :straight t
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(use-package tree-sitter-langs
	:straight t
	:defer t)

(use-package scopeline
  :straight (:host github :repo "jeremyf/scopeline.el")
  :hook ((ruby-mode ruby-ts-mode) . scopeline-mode)
  :config
  (setq scopeline-overlay-prefix " ---> "))

(use-package plz
  :straight t
	:defer t)

(use-package cl-lib
	:straight t)

(use-package sly
  :straight t
	:defer t
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package geiser
	:straight t
	:defer t
	:config
	(setq geiser-active-implementations '(racket)))

(use-package geiser-eros
  :after (eros geiser)
  :straight '(:type git :host sourcehut :repo "sokolov/geiser-eros")
	:defer t
  :config
  (setq geiser-mode-eval-last-sexp-to-buffer nil)
  (geiser-eros-mode 1))

(use-package geiser-racket
	:straight t
	:defer t)

(use-package geiser-mit
	:straight t
	:defer t)

;; (use-package scss-mode
;;   :straight t)

(use-package typescript-mode
	:straight t
  :mode ("\\.ts[x]\\'" . typescript-mode))

(use-package coffee-mode
	:straight t
  :mode "\\.coffee\\'"
  :config
  (setq coffee-indent-like-python-mode t
    coffee-tab-width 2))

(use-package emmet-mode
  :straight t
  :hook ((web-mode . emmet-mode)
				  (js2-mode . emmet-mode))
  :config
  (add-to-list 'emmet-jsx-major-modes 'js2-mode)
  (setq emmet-indentation 2))

(use-package ruby-tools
  :straight t)

(use-package yafolding
  :straight t
	:hook ((prog-mode . yafolding-mode)))

;; (use-package ruby-end
;; 	:straight t
;; 	:config
;; 	(setq ruby-end-insert-newline nil))

(use-package rails-i18n
	:straight t
	:config
	(setq rails-i18n-use-double-quotes t))

(use-package ruby-mode
  :straight t
  :mode
  "Appraisals\\'"
  "Berksfile\\'"
  "Brewfile\\'"
  "Capfile\\'"
  "Gemfile\\'"
  "Guardfile\\'"
  "Podfile\\'"
  "Puppetfile\\'"
  "Rakefile\\'"
  "Thorfile\\'"
  "Vagrantfile\\'"
  "\\.cap\\'"
  "\\.gemspec\\'"
  "\\.jbuilder\\'"
  "\\.podspec\\'"
  "\\.rabl\\'"
  "\\.rake\\'"
  "\\.ru\\'"
  "\\.thor\\'"
  "\\.rb\\'"
  "\\.pryrc\\'"
  :bind
  ((("C-c C-c" . ruby-send-region)
     ("RET" . newline-and-indent)
     ([(meta down)] . forward-sexp)
     ([(meta up)] . backward-sexp)))
  :hook ((ruby-mode . yafolding-mode)
          (ruby-ts-mode . yafolding-mode))
  :config
  (setq ruby-insert-encoding-magic-comment nil
    ruby-deep-indent-paren nil
    ruby-align-to-stmt-keywords t
    ruby-method-params-indent nil
    ruby-align-chained-calls nil
		ruby-method-call-indent nil
    ruby-parenless-call-arguments-indent nil
    ruby-after-operator-indent nil
		ruby-block-indent nil
    ruby-indent-level 2))

(use-package quickrun
  :straight t)

;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
;;   :hook ((prog-mode . copilot-mode)
;;           (docker-compose-mode . copilot-mode))
;;   :config
;; 	(setq copilot-node-executable "~/.asdf/installs/nodejs/18.19.0/bin/node"
;;     copilot-indent-offset-warning-disable t
;;     copilot-max-char -1)
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :config
  (setq chatgpt-shell-chatgpt-streaming nil
				chatgpt-shell-model-version 1
        chatgpt-shell-openai-key
        (lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))

(use-package ob-chatgpt-shell
  :requires chatgpt-shell
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("ob-chatgpt-shell.el"))
	:defer t)

(use-package imenu
	:straight nil
	:config
	(setq imenu-auto-rescan t))

(use-package yaml-imenu
  :straight t
  :config
  (yaml-imenu-enable))

(use-package multi-vterm
	:defer t
	:straight t
  :config
  (setq vterm-timer-delay 0.01))

(use-package so-long
  :straight t
  :config
  (global-so-long-mode)
	(setq so-long-threshold 2000))

(use-package eglot
  :straight (:type built-in)
  :hook ((go-mode . eglot-ensure)
          (elixir-mode . eglot-ensure))
  :config
  (setq eglot-stay-out-of '(eldoc company flymake)))

(use-package go-mode
  :straight t)

(use-package inf-ruby
  :straight t
  :after evil
  :config
  (setq comint-input-ring-file-name "~/.pry_history")
  (setq comint-input-ring-size 1000)
  (setq comint-input-ignoredups t)
  (define-key inf-ruby-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key 'normal inf-ruby-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key 'insert inf-ruby-mode-map (kbd "C-r") 'consult-history)
	(evil-define-key 'normal inf-ruby-mode-map (kbd "RET") 'comint-send-input)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'inf-ruby-mode-hook (lambda ()
                                  (comint-read-input-ring 'silent)))
  (add-hook 'inf-ruby-mode-hook (lambda ()
                                  (add-hook 'kill-buffer-hook 'comint-write-input-ring nil t))))

(use-package rubocop
  :straight t
	:defer t
  :config
  (setq rubocop-prefer-system-executable t))

(use-package bundler
  :straight t
	:defer t)

(use-package asdf
  :straight (:type git :host github :repo "tabfugnic/asdf.el" :branch "main")
  :config
  (asdf-enable))

;; (use-package minitest
;; 	:straight t
;; 	:config
;; 	(setq minitest-use-rails t))

(use-package rspec-mode
  :straight t
  :hook ((ruby-mode . rspec-mode)
				  (ruby-ts-mode . rspec-mode)
          (dired-mode . rspec-dired-mode))
  :config
	(setenv "FEATURE" "true")
  (setenv "SHOW_SQL" "true")
	(setenv "RUBYOPT" "-W:no-deprecated -W:no-experimental")
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
	(add-hook 'rspec-compilation-mode-hook (lambda ()
                                           (visual-line-mode t)))
	;; (setq rspec-use-spring-when-possible t)
  (setq rspec-use-docker-when-possible nil)
  (setq rspec-primary-source-dirs '("app"))
  (defun rspec-coverage-verify-file ()
    (interactive)
    (let ((original-feature (getenv "COVERAGE")))
      (setenv "COVERAGE" "true")
      (unwind-protect
        (rspec-verify)
        (setenv "COVERAGE" original-feature))))

  (defun rspec-coverage-verify-single ()
    (interactive)
    (let ((original-feature (getenv "COVERAGE")))
      (setenv "COVERAGE" "true")
      (unwind-protect
        (rspec-verify-single)
        (setenv "COVERAGE" original-feature)))))

(use-package haml-mode
  :straight t
  :defer t
  :mode
  ("\\.haml\\|.hamlc\\'" . (lambda ()
                             (haml-mode)
                             (setq evil-shift-width 2))))

(use-package sql
	:straight (:type built-in)
	:config
  (setq sql-product 'postgres)
	(setq sql-postgres-login-params
    '((user :default "postgres")
       (database :default "postgres")
       (server :default "localhost")
       (port :default 5432))))

(use-package sql-indent
  :straight t
  :hook (sql-mode . sqlind-minor-mode)
  :config
  (sqlind-setup-style-left))

(use-package sqlformat
  :straight t
  :hook (sql-mode . sqlformat-on-save-mode)
  :bind (:map sql-mode-map
          ("C-c C-f" . sqlformat))
  :init
  (setq sqlformat-command 'pgformatter
    sqlformat-args '("-s2" "-g")))

(use-package sqlup-mode
  :straight t
  :hook (sql-mode . sqlup-mode))

(use-package sql-clickhouse
  :straight t
  :config
  (setq sql-clickhouse-program "docker exec -it work_clickhouse_1 clickhouse-client"))

(use-package web-mode
  :demand t
  :straight t
  :config
  (setq-default
    web-mode-markup-indent-offset 2
    web-mode-css-indent-offset 2
    web-mode-code-indent-offset 2
    web-mode-comment-style 2
    web-mode-script-indent-offset 2
    web-mode-script-padding 2
	  web-mode-style-padding 0
	  web-mode-script-padding 0
    web-mode-enable-auto-pairing t
    web-mode-enable-auto-closing t
    web-mode-enable-auto-quoting t
    web-mode-enable-auto-expanding t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode)))

(use-package rjsx-mode
  :defer t
  :straight (:build t)
  :after compile
  :mode "\\.[mc]?jsx?\\'"
  :mode "\\.es6\\'"
  :mode "\\.pac\\'"
  :interpreter "node"
  :init
  (add-to-list 'compilation-error-regexp-alist 'node)
  (add-to-list 'compilation-error-regexp-alist-alist
    '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)"
       2 3 4))
  :config
  (setq js-chain-indent                  t
    js2-basic-offset                 2
    ;; ignore shebangs
    js2-skip-preprocessor-directives t
    js2-mode-show-parse-errors       nil
    js2-mode-show-strict-warnings    nil
    js2-strict-missing-semi-warning  nil
    js2-highlight-level              3
    js2-idle-timer-delay             0.15))

(use-package prettier-js
  :defer t
  :straight t
  :hook ((rjsx-mode typescript-mode) . prettier-js-mode)
  :config
  (setq prettier-js-args '("--double-quote" "--jsx-double-quote" "--no-semi")))

(use-package flycheck
  :straight t
  :defer 2
	:hook ((ruby-mode . flycheck-mode))
  :config
  (setq-default flycheck-disabled-checkers '(ruby-reek)))

(use-package add-node-modules-path
	:straight t
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package dumb-jump
  :straight t
  :config
  (setq dumb-jump-default-project "~/code/work"
		dumb-jump-prefer-searcher 'rg
    dumb-jump-force-searcher 'rg
    dumb-jump-selector 'completing-read
    xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package docker-compose-mode
  :straight t
	:defer t)

(use-package dockerfile-mode
  :straight t
	:defer t)

(use-package docker
	:straight t
  :defer t
  :config
  (setq docker-container-columns '(
                                    (:name "Id" :width 16 :template "{{ json .ID }}" :sort nil :format nil)
                                    (:name "Names" :width 30 :template "{{ json .Names }}" :sort nil :format nil)
                                    (:name "Image" :width 15 :template "{{ json .Image }}" :sort nil :format nil)
                                    (:name "Command" :width 25 :template "{{ json .Command }}" :sort nil :format nil)
                                    (:name "Status" :width 20 :template "{{ json .Status }}" :sort nil :format nil)
                                    (:name "Ports" :width 10 :template "{{ json .Ports }}" :sort nil :format nil)
                                    (:name "Created" :width 23 :template "{{ json .CreatedAt }}" :sort nil :format
	                                    (lambda
	                                      (x)
	                                      (format-time-string "%F %T"
			                                    (date-to-time x)))))))

(use-package bufler
  :straight (bufler :build t
              :files (:defaults (:exclude "helm-bufler.el")))
  :defer t)

(defun eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(defun load-frameg ()
  "Load ~/.emacs.frameg which should load the previous frame's geometry."
  (let ((frameg-file (expand-file-name "~/.emacs.frameg")))
    (when (file-readable-p frameg-file)
      (load-file frameg-file))))

(defun save-frameg ()
  "Get the current frame's geometry and save to ~/.emacs.frameg."
  (let ((frameg-font (frame-parameter (selected-frame) 'font))
         (frameg-left (frame-parameter (selected-frame) 'left))
         (frameg-top (frame-parameter (selected-frame) 'top))
         (frameg-width (frame-parameter (selected-frame) 'width))
         (frameg-height (frame-parameter (selected-frame) 'height))
         (frameg-file (expand-file-name "~/.emacs.frameg")))
    (with-temp-buffer
      ;; Turn off backup for this file
      (make-local-variable 'make-backup-files)
      (setq make-backup-files nil)
      (insert
        ";;; This file stores the previous emacs frame's geometry.\n"
        ";;; Last generated " (current-time-string) ".\n"
        "(setq initial-frame-alist\n"
        ;; " '((font . \"" frameg-font "\")\n"
        " '("
        (format " (top . %d)\n" (max frameg-top 0))
        (format " (left . %d)\n" (max frameg-left 0))
        (format " (width . %d)\n" (max frameg-width 0))
        (format " (height . %d)))\n" (max frameg-height 0)))
      (when (file-writable-p frameg-file)
        (write-file frameg-file)))))

(defun set-zshrc-env ()
  "Set local env."
  (setenv "PGHOST" "localhost"))

(if window-system
  (progn
    (add-hook 'after-init-hook 'set-zshrc-env)
    (add-hook 'after-init-hook 'load-frameg)
    (add-hook 'kill-emacs-hook 'save-frameg)
		(add-hook 'kill-emacs-hook 'kill-all-processes-on-exit)))

(defun split-and-follow-horizontally ()
	"Split and follow horizontally."
	(interactive)
	(split-window-below)
	(balance-windows)
	(other-window 1))

(defun split-and-follow-verticaly ()
	"Split and follow horizontally."
	(interactive)
	(split-window-right)
	(balance-windows)
	(other-window 1))

(defun current-location ()
  "Show the current location and put it into the kill ring.
Use the filename relative to the current VC root directory."
  (interactive)
  (let* ((file-name (file-relative-name buffer-file-name (vc-root-dir)))
	        (line-number (line-number-at-pos nil t))
	        (location (format "%s:%s" file-name line-number)))
    (kill-new location)
    (message location)))

(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                (not (equal f ".."))
                (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun yad/delete-window ()
  (interactive)
  (let ((current (current-buffer)))
    (when buffer-file-name
      (save-buffer))
    (delete-window)
    (unless (get-buffer-window current 'visible)
      (kill-buffer current))))

(update-to-load-path (expand-file-name "lisp" user-emacs-directory))

(require 'services)
(require 'scratch-buffers)
(require 'orgs)
(require 'rails-log-mode)
;; (require 'project-theme-colors)

(setq coffee-indent-like-python-mode t
  coffee-tab-width 2)

(defvar docker-containers-names '("work-billing-1" "work-billing-sidekiq-1"))

(defun attach-to-docker-container (container-namer)
  (interactive (list
		(completing-read "Container name: " docker-containers-names)))
  (docker-run-docker-async-with-buffer "attach" container-namer))


(global-set-key [escape] 'keyboard-escape-quit)

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-verticaly)

(defun safe-local-variable-p (sym val)
  "Put your guard logic here, return t when sym is ok, nil otherwise"
  t)
