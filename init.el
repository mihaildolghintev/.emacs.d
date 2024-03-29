(defvar-local compilation--start-time nil
  "The time when the compilation started as returned by `float-time'.")

(eval-after-load "bytecomp"
  '(add-to-list 'byte-compile-not-obsolete-vars
                'font-lock-beginning-of-syntax-function))

(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(use-package straight)


(use-package benchmark-init
  :straight t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package explain-pause-mode
  :straight (:host github
           :repo "lastquestion/explain-pause-mode")
  :commands explain-pause-mode
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
  :bind ("C-=" . er/expand-region)
  :config
  (evil-set-command-property 'er/expand-region :jump t))

(use-package ns-auto-titlebar
  :straight t
	:disabled t
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))

(use-package autorevert
  :straight nil
  :hook ((message-mode . auto-revert-tail-mode)
         (display-warning . auto-revert-tail-mode))
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-check-vc-info t
				auto-revert-interval 1
				auto-revert-use-notify nil
        auto-revert-verbose nil)
  (global-auto-revert-mode t))

(use-package compile
  :straight nil
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

(use-package emacs
  :straight nil
  :config
	(fset 'undo-auto-amalgamate 'ignore)
  (setq-default kill-buffer-delete-auto-save-files t
								redisplay-dont-pause t
								truncate-lines t
								sentence-end-double-space nil
								initial-scratch-message nil
								initial-major-mode 'emacs-lisp-mode
								tab-width 2
								mouse-highlight t
								bidi-paragraph-direction 'left-to-right
								redisplay-skip-fontification-on-input t
								cursor-in-non-selected-windows nil
								frame-title-format "Emacs"
								x-underline-at-descent-line t
								history-delete-duplicates t
								history-length t
								auto-window-vscroll nil)
	(setq undo-limit 6710886400
				undo-strong-limit 100663296
				ndo-outer-limit 1006632960
				highlight-nonselected-windows nil))

(use-package emacs
	:demand t
	:if (native-comp-available-p)
	:config
	(setq native-comp-speed 2))

(setq
 inhibit-compacting-font-caches t
 message-log-max 16384
 package-enable-at-startup nil
 load-prefer-newer t)

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

(use-package window
  :straight nil
  :config
  (setq split-height-threshold 99999999999999999))

(use-package message
  :straight nil
  :config
  (setq message-kill-buffer-on-exit t))

(use-package defaults
  :defer t
  :preface
  (setq-default
   c-basic-offset 2
   standard-indent 2)
  (setq
   ring-bell-function 'ignore
   insert-directory-program "gls"

   enable-recursive-minibuffers t
   confirm-kill-emacs 'y-or-n-p)

  (when (eq system-type 'darwin)
		(setq mac-command-modifier 'meta
					mac-option-modifier nil
					mac-control-modifier 'control)))

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(add-function :after after-focus-change-function
							(defun jf/garbage-collect-maybe ()
								(unless (frame-focus-state)
									(garbage-collect))))

(setq window-divider-default-right-width 8)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)
(global-subword-mode 1)

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


(global-set-key (kbd "M-o") 'other-window)

(use-package fontaine
  :straight t
  :config
  (setq fontaine-presets
				'((compact
					 :default-height 120)
					(default
					 :default-height 120)
					(t
					 :default-family "Berkeley Mono"
					 :default-weight regular
					 :default-height 110
					 :fixed-pitch-family nil ; falls back to :default-family
					 :fixed-pitch-weight nil ; falls back to :default-weight
					 :fixed-pitch-height 1.0
					 :fixed-pitch-serif-family nil ; falls back to :default-family
					 :fixed-pitch-serif-weight nil ; falls back to :default-weight
					 :fixed-pitch-serif-height 1.0
					 :variable-pitch-family "ETBembo"
					 :variable-pitch-weight nil
					 :variable-pitch-height 1.0
					 :bold-family nil ; use whatever the underlying face has
					 :bold-weight medium
					 :italic-family nil
					 :italic-slant italic
					 :line-spacing nil)))
  (fontaine-set-preset 'default))

(global-prettify-symbols-mode)

(use-package prog-mode
	:straight (:type built-in)
	:config
	(add-to-list 'prettify-symbols-alist '("def" . 955)))

(use-package dired
  :straight nil
  :config
  (setq dired-dwim-target t
        dired-recursive-copies t
				dired-recursive-deletes t
				dired-listing-switches "-laGhpX"
        dired-auto-revert-buffer t
				dired-use-ls-dired t
        dired-kill-when-opening-new-dired-buffer t)
	)

(use-package display-line-numbers
  :straight t
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :config
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode))

(use-package mood-line
  :straight t
  :hook ((after-init . mood-line-mode))
	:init
	(setq display-time-default-load-average nil
				display-time-format "%I:%M %p %e %b"))

(display-battery-mode 1)



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
  (setq delete-old-versions t)
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

(use-package company
  :straight t
  :config
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'magit-mode-hook #'company-mode)
  (setq company-format-margin-function 'company-dot-icons-margin
        company-dot-icons-format            " ● "
        company-minimum-prefix-length 1
        company-tooltip-limit 8
        company-tooltip-minimum-width 40
        company-tooltip-margin 2
        company-tooltip-offset-display 'lines
        company-idle-delay 0
        company-echo-delay 0
        company-dabbrev-code-everywhere t
        company-dabbrev-ignore-case nil
        company-dabbrev-other-buffers nil)

  (global-set-key (kbd "M-i") 'consult-company)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

(use-package company-box
  :straight t
	:after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-enable-icon nil
        company-box-scrollbar nil
        company-box-minimum-width 40
        company-box-doc-enable nil))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
	:defer t
  :config
  (setq chatgpt-shell-chatgpt-streaming nil
				chatgpt-shell-model-version 2
        chatgpt-shell-openai-key
        (lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))

(use-package ob-chatgpt-shell
  :requires chatgpt-shell
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("ob-chatgpt-shell.el"))
	:defer t)

(use-package eldoc
  :straight nil
  :config
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package simple
  :bind (("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-this-buffer)
         ("C-h C-f" . describe-face)
         ([remap undo] . undo-only))
  :hook ((before-save . delete-trailing-whitespace))
  :config
  (setq indent-tabs-mode nil
        yank-excluded-properties t
        blink-matching-delay 0
        blink-matching-paren nil
        copy-region-blink-delay 0
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
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  (vertico-mode))

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

(use-package ef-themes
  :straight (:host github
                   :repo "protesilaos/ef-themes"
                   :branch "main")
	:config
	(setq ef-themes-common-palette-overrides
				'((border-mode-line-active black))))


(use-package modus-themes
  :straight t
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)
  (setq modus-themes-common-palette-overrides
				'((bg-mode-line-active bg-yellow-subtle)
					(fg-mode-line-active fg-main)
					(border-mode-line-active yellow-intense))))

(load-theme 'modus-operandi t)


(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
	:bind (("C-c g" . consult-ripgrep)
				 ("C-c l" . consult-git-log-grep)
				 ("C-c n" . create-scratch-buffer))
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
  (setq consult-project-root-function #'projectile-project-root))


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
  (global-set-key (kbd "C-h d") 'devdocs-lookup)

  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local devdocs-current-docs '("elisp"))))
  (add-hook 'css-mode-hook
            (lambda () (setq-local devdocs-current-docs '("css"))))
  (add-hook 'coffee-mode-hook
            (lambda () (setq-local devdocs-current-docs '("coffeescript~2"))))
  (add-hook 'ruby-mode-hook
            (lambda () (setq-local devdocs-current-docs '("ruby~2.7" "rails~6.1")))))

(use-package deadgrep
  :straight t
	:defer t
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
	:defer t
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

(use-package jq-mode
	:straight t
	:config
	(add-to-list 'auto-mode-alist '("\\.jq$" . jq-mode))
	(setq hurl-variables-file "~/code/work/restapi/api_local_vars.json"))

(use-package csv-mode
  :straight t
  :defer t
  :custom
  (csv-align-max-width 80))

(use-package puni
  :straight t)

(use-package pocket-reader
	:straight t)

(use-package elfeed
  :straight t
	:defer t
  :config
  (setq elfeed-feeds
				'("http://nullprogram.com/feed/"
					"https://planet.emacslife.com/atom.xml"
					"https://allaboutcoding.ghinda.com/rss.xml"
					"https://mensfeld.pl/feed/"
					"https://takeonrules.com/index.xml"
					"https://arialdomartini.github.io/feed.xml"
					"https://rubypilot.com/"
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
          help-mode
					helpful-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

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

  (evil-define-key 'normal 'global (kbd "M-w") 'delete-window)

  (evil-define-key 'visual 'global (kbd "M-[") 'puni-wrap-square)
  (evil-define-key 'visual 'global (kbd "{") 'puni-wrap-curly)
  (evil-define-key 'visual 'global (kbd "(") 'puni-wrap-round)

  (evil-define-key 'visual 'global (kbd "K") 'drag-stuff-up)
  (evil-define-key 'visual 'global (kbd "J") 'drag-stuff-down)

  (evil-define-key 'visual 'global (kbd "M-(") 'puni-wrap-round)
  (evil-define-key 'visual 'global (kbd "M-[") 'puni-wrap-square)
  (evil-define-key 'visual 'global (kbd "M-{") 'puni-wrap-curly)

  (evil-define-key 'normal 'global (kbd "<leader>ka") 'paredit-splice-sexp-killing-backward)
  (evil-define-key 'normal 'evil-matchit-mode-map "m" 'evilmi-jump-items))

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

(use-package elec-pair
  :straight (:type built-in)
  :config
	(setq electric-pair-preserve-balance t
				electric-pair-delete-adjacent-pairs nil
				electric-pair-open-newline-between-pairs t)
  (electric-pair-mode))

;; (use-package smartparens-mode
;; 	:straight smartparens
;; 	:hook ((prog-mode . turn-on-smartparens-mode)
;;          (markdown-mode . turn-on-smartparens-mode)
;;          (org-mode . turn-on-smartparens-mode)
;;          (prog-mode . turn-on-show-smartparens-mode)
;;          (markdown-mode . turn-on-show-smartparens-mode)
;;          (org-mode . turn-on-show-smartparens-mode)
;;          (emacs-lisp-mode . turn-on-smartparens-strict-mode))
;;   :config
;;   (require 'smartparens-config)
;; 	(sp-with-modes 'emacs-lisp-mode
;; 		;; disable ', it's the quote character!
;; 		(sp-local-pair "'" nil :actions nil)
;; 		;; also only use the pseudo-quote inside strings where it
;; 		;; serves as hyperlink.
;; 		(sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))))

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


(use-package wsd-mode
  :straight t)

(use-package closql
  :straight t)

(use-package vc-hooks
  :config
  (setq vc-follow-symlinks t))

(use-package magit
  :straight t
  :custom
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :config
  (magit-auto-revert-mode)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
	;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
	(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
	(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (setq magit-no-confirm '(stage-all-changes unstage-all-changes)
				auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (global-set-key (kbd "C-c m") 'magit-status))

(use-package diff-hl
	:straight t
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

(use-package ghub
	:straight t
	:config
	(setq ghub-default-host "git.saltedge.com/api/v4"))

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
  :straight t
  :defer t)

(use-package which-key
  :straight t
  :config
  (which-key-mode)
  (setq which-key-max-display-columns 2
        which-key-add-column-padding 10
        which-key-max-description-length 120))



(use-package comment-dwim-2
	:straight t
	:bind (("M-;" . comment-dwim-2)))


(use-package uniquify
	:straight nil
  :init
  (setq uniquify-buffer-name-style 'forward
        uniquify-min-dir-content 2))

;; (use-package prettier
;;   :straight t
;;   :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
;; 	:config
;; 	(setq prettier-prettify-on-save-flag nil))



(use-package lin
  :straight t
  :config
  (add-hook 'dired-sidebar-mode-hook 'lin-mode))

(use-package tree-sitter
  :straight t
	:defer t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
	:straight t
	:defer t)

(use-package scopeline
  :straight (:host github :repo "jeremyf/scopeline.el")
  :hook ((ruby-mode ruby-ts-mode) . scopeline-mode))

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

(use-package ruby-end
	:straight t
	:config
	(setq ruby-end-insert-newline nil))

(use-package rails-i18n
	:straight t
	:config
	(setq rails-i18n-use-double-quotes t))

(use-package go-mode
	:straight t)

;; (use-package lsp-mode
;; 	:straight t
;;   :hook ((go-mode . lsp)
;; 				 (ruby-mode . lsp))
;; 	:config
;; 	(setq lsp-headerline-breadcrumb-enable nil
;; 				lsp-modeline-code-actions-enable nil
;; 				lsp-solargraph-use-bundler t
;; 				lsp-solargraph-library-directories '("~/.asdf")
;; 				lsp-modeline-diagnostics-enable nil))

;; (use-package lsp-ui
;; 	:straight t)

;; (use-package eglot
;; 	:straight nil
;; 	:hook ((ruby-mode . eglot-ensure)
;; 				 (js2-mode . eglot-ensure))
;; 	:config
;; 	(add-to-list 'eglot-server-programs '(ruby-mode . ("bundle" "exec" "solargraph" "stdio")))
;; 	(setq eglot-ignored-server-capabilities '(:inlayHintProvider
;; 																						:documentHighlightProvider
;; 																						:codeLensProvider
;; 																						:workspaceSymbolProvider
;; 																						:documentSymbolProvider)))


(use-package tramp
	:straight (:type built-in)
	:defer t)

(use-package hurl-mode
	:straight (:host github :repo "jaszhe/hurl-mode")
	:config
	(add-to-list 'auto-mode-alist '("\\.hurl\\'" . hurl-mode)))

(use-package restclient
	:straight t)

(use-package impostman
	:straight t)

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
    ("RET" . newline-and-indent)))
  :config
  (add-hook 'ruby-mode-hook 'yafolding-mode)
  (setq ruby-insert-encoding-magic-comment nil
        ruby-deep-indent-paren nil
        ruby-align-to-stmt-keywords t
        ruby-align-chained-calls nil
				ruby-method-call-indent nil
				ruby-block-indent nil
        ruby-indent-level 2))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
  :hook ((ruby-mode ruby-ts-mode js-mode coffee-mode) . copilot-mode)
  :config
	(setq copilot-node-executable "~/.asdf/installs/nodejs/18.19.0/bin/node")
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package imenu
	:straight nil
	:config
	(setq imenu-auto-rescan t))

(use-package yaml-imenu
  :straight t
  :config
  (yaml-imenu-enable))

(use-package eshell
  :straight t
	:defer t
  :config
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color"))))

(use-package multi-vterm
	:defer t
	:straight t)

(use-package ansi-color
  :straight t)

(use-package so-long
  :straight t
  :config
  (global-so-long-mode)
	(setq so-long-threshold 2000))

(use-package inf-ruby
  :straight t
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
	:defer t
  :hook ((ruby-mode . rspec-mode)
				 (ruby-ts-mode . rspec-mode)
         (dired-mode . rspec-dired-mode))
  :config
	(setenv "FEATURE" "true")
	(setenv "RUBYOPT" "-W:no-deprecated -W:no-experimental")
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
	(add-hook 'rspec-compilation-mode-hook (lambda ()
                                       (visual-line-mode t)))
	(setq rspec-use-spring-when-possible t)
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
	(setq sql-postgres-login-params
      '((user :default "postgres")
        (database :default "postgres")
        (server :default "localhost")
        (port :default 5432))))

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
	:hook ((ruby-mode . flycheck-mode))
  :config
  (setq-default flycheck-disabled-checkers '(ruby-reek))
	(setq flycheck-rubocoprc "~/code/work/bucket/.rubocop.yml")
	)


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
	:defer t)

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

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(define-key dired-mode-map (kbd "?") 'dired-get-size)

(update-to-load-path (expand-file-name "lisp" user-emacs-directory))

(require 'services)
(require 'scratch-buffers)
(require 'orgs)
(require 'rails-log-mode)
(require 'project-theme-colors)

(setq coffee-indent-like-python-mode t
      coffee-tab-width 2)


(global-set-key [escape] 'keyboard-escape-quit)

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-verticaly)

(defun safe-local-variable-p (sym val)
"Put your guard logic here, return t when sym is ok, nil otherwise"
  t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((rspec-use-docker-when-possible . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
