(use-package use-package
  :no-require
  :custom
  (use-package-enable-imenu-support t))

(use-package early-init
  :no-require
  :unless (featurep 'early-init)
  :config
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(load-file (expand-file-name "tree-surgeon-split-join.el" user-emacs-directory))
(load-file (expand-file-name "services.el" user-emacs-directory))

(use-package straight)

(use-package org
  :straight t)

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region)
  :config
  (evil-set-command-property 'er/expand-region :jump t))

(use-package ns-auto-titlebar
  :straight t
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))

(use-package golden-ratio
  :straight t
  :config
  (golden-ratio-mode 1))

(global-auto-revert-mode t)

(add-hook 'message-mode-hook
          #'auto-revert-tail-mode)

(add-hook 'display-warning #'auto-revert-tail-mode)

;; Revert Dired and other buffers
(setq-default global-auto-revert-non-file-buffers t)

;; As it says really
(setq-default compilation-scroll-output t)

(setq-default kill-buffer-delete-auto-save-files t)

;; Set the directory where backups should go
(setq-default backup-directory-alist
              '(("." . "~/.cache/emacs/auto-save"))
              backup-by-copying t
              delete-old-versions t)

;; Use a similar directory for undo-tree history
(setq-default undo-tree-history-directory-alist
              '(("." . "~/.cache/emacs/undo-history")))

(setq-default diff-add-log-use-relative-names t)

;; Smoother Scrolling for newer Emacs clients.
(pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-large-scroll-height 30
      pixel-scroll-precision-use-momentum nil)

(use-package comment-dwim-2
  :straight t
  :config
  (global-set-key (kbd "M-;") 'comment-dwim-2)
  (define-key org-mode-map (kbd "M-;") 'org-comment-dwim-2) ;
  (setq cd2/region-command 'cd2/comment-or-uncomment-lines
        cd2/region-command 'cd2/comment-or-uncomment-region))


(use-package defaults
  :defer t
  :preface
  (setq-default
   indent-tabs-mode nil
   load-prefer-newer t
   redisplay-dont-pause t
   split-height-threshold nil
   truncate-lines t
   message-kill-buffer-on-exit t
   delete-old-versions t
   bidi-paragraph-direction 'left-to-right
   cursor-in-non-selected-windows nil
   auto-revert-check-vc-info t
   auto-revert-verbose nil
   frame-title-format "Emacs"
   auto-window-vscroll nil
   indent-tabs-mode nil
   tab-width 2
   c-basic-offset 2
   standart-indent 2
   mouse-highlight t
   hscroll-step 1
   hscroll-margin 1
   scroll-margin 0
   scroll-preserve-screen-position nil
   js-indent-level 2
   js-jsx-indent-level 2
   standard-indent 2
   frame-resize-pixelwise window-system
   window-resize-pixelwise window-system)
  (setq
   ring-bell-function 'ignore
   insert-directory-program "gls"
   dired-listing-switches "-a -g --group-directories-first --human-readable --no-group"
   enable-recursive-minibuffers t
   confirm-kill-emacs 'y-or-n-p)
  (set-face-attribute 'default nil :family "Berkeley Mono" :height 140)
  (when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
        mac-option-modifier nil
        mac-control-modifier 'control)
  (global-set-key [wheel-right] (lambda ()
                                  (interactive)
                                  (scroll-left 4)))
  (global-set-key [wheel-left] (lambda ()
                                 (interactive)
                                 (scroll-right 4))))
  (defun kos/keyboard-quit ()
    "Quit out of whatever."
    (interactive)
    ;; Delete frame if it is a minbuffer only popup
    (if (and (equal (cdr (assq 'name (frame-parameters))) "emacs-popup")
             (equal (cdr (assq 'minibuffer (frame-parameters))) 'only))
        (delete-frame))
    (keyboard-escape-quit)
    (keyboard-quit)
    ))

(add-hook 'before-make-frame-hook
          #'(lambda ()
              (add-to-list 'default-frame-alist '(left   . 0))
              (add-to-list 'default-frame-alist '(top    . 0))
              (add-to-list 'default-frame-alist '(height . 80))
              (add-to-list 'default-frame-alist '(width  . 310))))



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

;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "M-o") 'other-window)

(use-package dired
  :straight nil
  :config
  (setq dired-dwim-target t
        dired-recursive-copies t
        dired-auto-revert-buffer t
        dired-kill-when-opening-new-dired-buffer t))

(use-package display-line-numbers
  :straight t
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  :config
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)
  (add-hook 'conf-mode-hook #'display-line-numbers-mode))

  (setq-default mode-line-buffer-identification
              (let ((orig  (car mode-line-buffer-identification)))
                `(:eval (cons (concat ,orig (abbreviate-file-name default-directory))
                              (cdr mode-line-buffer-identification)))))
(use-package mood-line
  :defer t
  :straight t
  :hook (after-init . mood-line-mode))

(use-package indent-guide
  :straight t
  :config
  (add-hook 'ruby-mode-hook 'indent-guide-mode)
  (add-hook 'ruby-mode-hook 'indent-guide-mode)
  (add-hook 'ruby-ts-mode-hook 'indent-guide-mode)
  (add-hook 'coffee-mode-hook 'indent-guide-mode)
  (add-hook 'haml-mode-hook 'indent-guide-mode))

(use-package hungry-delete
  :straight t
  :config
  (setq hungry-delete-except-modes '(coffee-mode haml-mode))
  (global-hungry-delete-mode t))

(use-package startup
  :no-require
  :custom
  (inhibit-splash-screen t)
  (user-mail-address "mihail.dolghintev@saltedge.com")
  (user-full-name "Mihail Dolghintev"))

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
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t)))

(use-package subr
  :no-require
  :init
  (fset 'yes-or-no-p 'y-or-n-p))

(use-package savehist
  :hook (after-init . savehist-mode))

(use-package super-save
  :straight t
  :config
  (super-save-mode +1))

(use-package saveplace
  :init
  (save-place-mode t))

(use-package company
  :straight t
  :config
  (add-hook 'prog-mode-hook #'company-mode)
  (add-hook 'web-mode-hook #'company-mode)
  (add-hook 'magit-mode-hook #'company-mode)
  (push 'company-elisp company-backends)
  (setq company-format-margin-function 'company-dot-icons-margin
        company-dot-icons-format            " ● "
        company-minimum-prefix-length 4
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
  :config
  (setq chatgpt-shell-chatgpt-streaming nil
        chatgpt-shell-openai-key
        (lambda ()
          (auth-source-pick-first-password :host "api.openai.com"))))


;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package select
  :no-require
  :when (display-graphic-p)
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el"))
  :config
  (setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))
  )

(use-package simple
  :bind (("M-z" . zap-up-to-char)
         ("M-S-z" . zap-to-char)
         ("C-x k" . kill-this-buffer)
         ("C-h C-f" . describe-face)
         ([remap undo] . undo-only))
  :hook ((before-save . delete-trailing-whitespace))
  :custom
  (yank-excluded-properties t)
  (blink-matching-delay 0)
  (blink-matching-paren t)
  (copy-region-blink-delay 0)
  (shell-command-default-error-buffer "*Shell Command Errors*"))

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
                   :branch "main"))

(use-package standard-themes
  :straight (:host github
                   :repo "protesilaos/standard-themes"
                   :branch "main"))

(use-package modus-themes
  :straight t)

(use-package hima-theme
  :straight t)

(load-theme 'ef-maris-light t)

(use-package eat
  :straight (:type git
                   :host codeberg
                   :repo "akib/emacs-eat"
                   :files ("*.el" ("term" "term/*.el") "*.texi"
                           "*.ti" ("terminfo/e" "terminfo/e/*")
                           ("terminfo/65" "terminfo/65/*")
                           ("integration" "integration/*")
                           (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  :config
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
      --smart-case --no-heading --line-number --hidden --follow --glob \"!.git/*\" .")
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
  :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-locate-args "mdfind -name")
  (setq consult-async-min-input 2)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-flycheck
  :straight t)

(use-package consult-git-log-grep
  :straight t
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

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
            (lambda () (setq-local devdocs-current-docs '("ruby~2.7" "rails~6.0"))))
  (add-hook 'ruby-mode-hook
            (lambda () (setq-local devdocs-current-docs '("ruby~2.7" "rails~6.0")))))

(use-package tldr
  :straight t)

(use-package deadgrep
  :straight t
  :config
  (global-set-key (kbd "C-c f") #'deadgrep))

(use-package symbol-overlay
  :straight t
  :config
  (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
  (global-set-key (kbd "M-N") 'symbol-overlay-jump-prev))

(use-package projectile
  :straight t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/code/"))
  (setq consult-project-function (lambda (_) (projectile-project-root))))


(use-package projectile-rails
  :straight t
  :after projectile
  :config
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)
  (projectile-rails-global-mode))

(use-package paren
  :hook (prog-mode . show-paren-mode))

(use-package css-mode
  :straight t
  :custom
  (css-indent-offset 2))

(use-package json-mode
  :straight t
  :custom
  (js-indent-level 2))

(use-package csv-mode
  :straight t
  :defer t
  :custom
  (csv-align-max-width 80))

(use-package puni
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
        evil-disable-insert-state-bindings t)

  :config
  (evil-mode t)
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

  (evil-define-key 'normal 'global (kbd "C-p") 'previous-line)
  (evil-define-key 'normal 'global (kbd "C-n") 'next-line)

  (evil-define-key 'normal 'global (kbd "M-w") 'delete-window)

  (evil-define-key 'visual 'global (kbd "[") 'paredit-wrap-square)
  (evil-define-key 'visual 'global (kbd "{") 'paredit-wrap-curly)

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

(use-package evil-escape
  :after evil
  :straight t
  :config
  (setq-default evil-esc-delay 0.25
                evil-escape-key-sequence "jk")
  (evil-escape-mode))

(use-package evil-matchit
  :straight t
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package drag-stuff
  :straight t)

(use-package symbol-overlay
  :straight t
  :config
  (global-set-key (kbd "M-n") 'symbol-overlay-jump-next)
  (global-set-key (kbd "M-N") 'symbol-overlay-jump-prev))

(use-package elec-pair
  :straight t
  :config
  (electric-pair-mode))

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

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-initialize))

(use-package wsd-mode
  :straight t)


(use-package magit
  :straight t
  :custom
  (magit-diff-refine-ignore-whitespace t)
  (magit-diff-refine-hunk 'all)
  :config
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (setq magit-no-confirm '(stage-all-changes
                           unstage-all-changes))
  (setq auto-revert-buffer-list-filter
      'magit-auto-revert-repository-buffer-p)
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (global-set-key (kbd "C-c m") 'magit-status)
  (global-set-key [(f12)] 'magit-status))


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
                   :italic t)))

  :config
  (global-blamer-mode t))

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
  (setq pulsar-face 'pulsar-red)
  (setq pulsar-highlight-face 'pulsar-red)
  (pulsar-global-mode 1))


(use-package helpful
  :straight t
  :defer t
  :config
  (global-set-key (kbd "C-h f") #'helpful-callable)

  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))

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

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :config
  (defun flycheck-node_modules-executable-find (executable)
    (or
     (let* ((base (locate-dominating-file buffer-file-name "node_modules"))
            (cmd  (if base (expand-file-name (concat "node_modules/.bin/" executable)  base))))
       (if (and cmd (file-exists-p cmd))
           cmd))
     (flycheck-default-executable-find executable)))

  (defun my-node_modules-flycheck-hook ()
    (setq-local flycheck-executable-find #'flycheck-node_modules-executable-find))

  (add-hook 'coffee-mode-hook 'my-node_modules-flycheck-hook)
  (setq flycheck-coffeelintrc "coffeelint.json")
  (setq-default flycheck-disabled-checkers '(ruby-reek))
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package prettier
  :straight t
  :defer t)

(use-package js2-mode
  :straight t
  :mode (("\\.js\\'" . js2-mode))
  :interpreter ("node" . js2-mode)
  :config
  (setq js2-use-font-lock-faces t
        js2-mode-must-byte-compile nil
        javascript-indent-level 2
        js2-basic-offset 2
        typescript-indent-level 2
        tab-width 2
        js2-strict-trailing-comma-warning nil
        js2-idle-timer-delay 0.5
        js2-skip-preprocessor-directives t
        js2-strict-inconsistent-return-warning nil ; return <=> return null
        js2-enter-indents-newline nil
        js2-bounce-indent-p t
        js2-strict-missing-semi-warning nil
        js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (js2-imenu-extras-setup))

(use-package js2-refactor
  :straight t
  :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c ."))

(use-package format-all
  :straight t)

(use-package dired-sidebar
  :straight t
  :config
  (setq dired-sidebar-width 60))

(use-package lin
  :straight t
  :config
  (add-hook 'dired-sidebar-mode-hook 'lin-mode))

;; (use-package tree-sitter
;;   :straight t
;;   :config
;;   (global-tree-sitter-mode))

;; (use-package tree-sitter
;;   :t straight
;;   :config
;;   (global-tree-sitter-mode))

;; (use-package tree-sitter-langs
;;   :straight t)

;; (use-package treesit-auto
;;   :straight t
;;   :config
;;   (global-treesit-auto-mode))

(use-package pdf-tools
  :straight t)

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package plz
  :straight t)

(use-package vue-mode
  :straight t)

(use-package sly
  :straight t
  :config
  (setq inferior-lisp-program "sbcl")
  (define-key sly-mode-map (kbd "C-c C-c") 'sly-eval-defun)
  (define-key sly-mode-map (kbd "C-c C-r") 'sly-eval-region)
  (define-key sly-mode-map (kbd "C-c C-b") 'sly-eval-buffer)
  )

(use-package css-mode
  :straight t)

(use-package scss-mode
  :straight t)

(use-package typescript-mode
  :straight t)

(use-package coffee-mode
  :straight t
  :config
  (setq coffee-indent-like-python-mode t
        coffee-tab-width 2))

(use-package emmet-mode
  :straight t
  :hook (web-mode . emmet-mode)
  :config
  (add-to-list 'emmet-jsx-major-modes 'js2-mode))

(use-package clojure-mode
  :straight t)

(use-package cider
  :straight t)

(use-package go-mode
  :straight t)

(use-package ruby-mode
  :straight t
  :bind
  ((("C-c C-c" . ruby-send-region)))
  :config
  (add-hook 'ruby-mode-hook #'subword-mode)
  ;; (add-hook 'ruby-mode-hook
  ;; (lambda ()
  ;;   (setq-local flycheck-command-wrapper-function
  ;;               (lambda (command) (append '("bundle" "exec") command))))
  ;; )
  (add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|.pryrc\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode))
  (setq ruby-insert-encoding-magic-comment nil

        ruby-align-to-stmt-keywords t
        ruby-align-chained-calls nil
        ruby-indent-level 2))

(use-package ruby-tools
  :straight t)

(use-package ruby-hash-syntax
  :after ruby-mode
  :straight t
  :config
  (ruby-tools-mode))

(use-package align
  :straight t
  :config
  (add-to-list 'align-rules-list
                 '(ruby-hash-values-colon
                   (regexp . ":\\(\\s-*\\)")
                   (group . 1)
                   (modes . '(ruby-mode))))
  (add-to-list 'align-rules-list
             '(ruby-hash-values
               (regexp . "=>\\(\\s-*\\)")
               (repeat . t)
               (modes . '(ruby-mode))))
  (add-to-list 'align-rules-list
                 '(ruby-hash-values
                   (regexp . ".to\\(\\s-*\\)")
                   (group . 1)
                   (modes . '(ruby-mode)))))

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :config
  (add-hook 'prog-mode-hook 'copilot-mode)
  (remove-hook 'lisp-mode-hook 'copilot-mode)
  (remove-hook 'sly-mode-hook 'copilot-mode)
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

(use-package imenu-list
  :straight t)

(use-package yaml-imenu
  :straight t
  :config
  (yaml-imenu-enable))

(use-package eshell
  :straight t
  :config
  (add-hook 'eshell-mode-hook (lambda () (setenv "TERM" "xterm-256color"))))

(use-package ansi-color
  :straight t)

(use-package perspective
  :straight t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package vterm
  :straight t
  :config
  (setq vterm-max-scrollback 200))

(use-package multi-vterm
  :straight t)

(use-package so-long
  :straight t
  :config
  (global-so-long-mode))

(use-package zoom-window
  :straight t)

(use-package golden-ratio
  :straight t)

;; (use-package robe
;;   :straight t
;;   :hook
;;   (ruby-mode . robe-mode)
;;   (ruby-ts-mode . robe-mode))

(use-package inf-ruby
  :straight t
  :config
  (setq comint-input-ring-file-name (format "%s%s" user-emacs-directory "inf-history"))
  (setq comint-input-ring-size 1000)
  (setq comint-input-ignoredups t)
  (define-key inf-ruby-mode-map (kbd "M-r") 'consult-history)
  (evil-define-key 'normal inf-ruby-mode-map (kbd "M-r") 'consult-history)
  (evil-define-key 'insert inf-ruby-mode-map (kbd "M-r") 'consult-history)
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'inf-ruby-mode-hook (lambda ()
                                  (comint-read-input-ring 'silent)))
  (add-hook 'inf-ruby-mode-hook (lambda ()
                                  (add-hook 'kill-buffer-hook 'comint-write-input-ring nil t)))
  )

(use-package rubocop
  :straight t
  :config
  (setq rubocop-prefer-system-executable t
        rubocop-autocorrect-command "rubocop -A --format emacs"))

(use-package bundler
  :straight t)

(use-package asdf
  :straight (:type git :host github :repo "tabfugnic/asdf.el" :branch "main")
  :config
  (asdf-enable)
  )

(use-package rspec-mode
  :straight t
  :hook '((ruby-base-mode . rspec-mode)
          (dired-mode . rspec-dired-mode))
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (setq rspec-primary-source-dirs '("app"))
  (define-key rspec-mode-map (kbd "C-c . v") 'rspec-feature-verify-file)
  (define-key rspec-mode-map (kbd "C-c . s") 'rspec-feature-verify-single)
  (defun rspec-feature-verify-single ()
    (interactive)
    (let ((original-feature (getenv "FEATURE")))
      (setenv "FEATURE" "true")
      (unwind-protect
          (rspec-verify-single)
        (setenv "FEATURE" original-feature))))

  (defun rspec-feature-verify-file ()
    (interactive)
    (let ((original-feature (getenv "FEATURE")))
      (setenv "FEATURE" "true")
      (unwind-protect
          (rspec-verify)
        (setenv "FEATURE" original-feature)))))

(use-package haml-mode
  :straight t
  :defer t
  :mode
  ("\\.haml\\|.hamlc\\'" . (lambda ()
                             (haml-mode)
                             (flycheck-mode -1)
                             (setq evil-shift-width 2))))

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
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-closing t
   web-mode-enable-auto-quoting t
   web-mode-enable-auto-expanding t)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode)))

(use-package dumb-jump
  :straight t
  :config
  (setq dumb-jump-prefer-searcher 'rg
        dumb-jump-force-searcher 'rg
        dumb-jump-selector 'completing-read
        xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package docker-compose-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

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

(defun set-ruby-no-opt ()
  "Dont display errors in ruby console."
  (setenv "RUBYOPT" "-W:no-deprecated -W:no-experimental"))

(defun set-zshrc-env ()
  "Set local env."
  (setenv "PGHOST" "localhost"))

(if window-system
    (progn
      (add-hook 'after-init-hook 'set-ruby-no-opt)
      (add-hook 'after-init-hook 'set-zshrc-env)
      (add-hook 'after-init-hook 'load-frameg)
      (add-hook 'kill-emacs-hook 'save-frameg)))



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

;; Reload/evaluate this file i.e .emacs after change
(defun reload-dotemacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))


(global-set-key [escape] 'kos/keyboard-quit)

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-verticaly)
