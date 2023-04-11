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

(use-package straight)

(use-package org
  :straight t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)))
  (setq org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
  )

(use-package expand-region
  :straight t
  :bind ("C-=" . er/expand-region)
  :config
  (evil-set-command-property 'er/expand-region :jump t))

(use-package ns-auto-titlebar
  :straight t
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))

(use-package autorevert
  :hook (after-init . global-auto-revert-mode))

(use-package defaults
  :defer t
  :preface
  (setq-default
   indent-tabs-mode nil
   load-prefer-newer t
   split-height-threshold nil
   truncate-lines t
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
   dired-listing-switches "-alh"
   enable-recursive-minibuffers t
   confirm-kill-emacs 'y-or-n-p)
  (set-face-attribute 'default nil :font "Berkeley Mono" :height 140 :weight 'regular)
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
    (keyboard-quit)))


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
  :defer t
  :straight t
  :hook (after-init . mood-line-mode))

(use-package startup
  :no-require
  :custom
  (inhibit-splash-screen t)
  (user-mail-address "mihail.dolghintev@saltedge.com")
  (user-full-name "Mihail Dolghintev"))

(use-package files
  :preface
  (defvar backup-dir
    (expand-file-name ".cache/backups" user-emacs-directory)
    "Directory to store backups.")
  (defvar auto-save-dir
    (expand-file-name ".cache/auto-save/" user-emacs-directory)
    "Directory to store auto-save files.")
  :custom
  (backup-by-copying t)
  (create-lockfiles nil)
  (backup-directory-alist
   `(("." . ,backup-dir)))
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
  (add-hook 'inf-ruby-mode-hook #'company-mode)
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

(use-package select
  :no-require
  :when (display-graphic-p)
  :custom
  (x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

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
  (setq completion-styles '(orderless)
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


(load-theme 'hima t)

(use-package consult
  :straight t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
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

(use-package consult-company
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
  :defer t
  :custom
  (css-indent-offset 2))

(use-package json-mode
  :defer t
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
  (define-key transient-map (kbd "<escape>") 'transient-quit-one)
  (global-set-key (kbd "C-c m") 'magit-status))

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
  :bind
  (:map sly-mode-map
        ("C-c C-c" . sly-eval-defun)
        ("C-c C-r" . sly-eval-region)
        ("C-c C-b" . sly-eval-buffer))
  :config
  (eros-mode 1)
  )

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
  (setq pulsar-highlight-face 'pulsar-yellow))


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
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package js2-refactor
  :straight t
  :init   (add-hook 'js2-mode-hook 'js2-refactor-mode)
  :config (js2r-add-keybindings-with-prefix "C-c ."))

(use-package xref-js2
  :straight t
  :config
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))

(use-package format-all
  :straight t)

(use-package dired-sidebar
  :straight t
  :config
  (setq dired-sidebar-width 60
        dired-sidebar-should-follow-file t))

(use-package lin
  :straight t
  :config
  (add-hook 'dired-sidebar-mode-hook 'lin-mode))

(use-package tree-sitter
  :straight t
  :config
  (add-hook 'ruby-mode-hook #'tree-sitter-mode)
  (add-hook 'js2-mode-hook #'tree-sitter-mode)
  (add-hook 'js2-jsx-mode-hook #'tree-sitter-mode)
  (add-hook 'js-mode-hook #'tree-sitter-mode)
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t)

(use-package ts-fold
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold")
  :config
  (global-ts-fold-mode)
  (global-set-key (kbd "C-c t") 'ts-fold-toggle))

(use-package vue-mode
  :straight t)

(use-package evil-textobj-tree-sitter
  :straight t
  :config
  ;; Goto start of next function
  (define-key evil-normal-state-map (kbd "]f") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;; Goto start of previous function
  (define-key evil-normal-state-map (kbd "[f") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;; Goto end of next function
  (define-key evil-normal-state-map (kbd "]F") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;; Goto end of previous function
  (define-key evil-normal-state-map (kbd "[F") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f"
              (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a"
              (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))))

(use-package sly
  :straight t
  :custom (inferior-lisp-program "sbcl"))

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


(use-package ruby-mode
  :straight t
  :bind
  ((("C-c C-c" . ruby-send-region)))
  :config
  (add-hook 'ruby-mode-hook #'subword-mode)
  (setq ruby-insert-encoding-magic-comment nil
        ruby-indent-level 2)
  (defun kill-ruby-instances ()
    (interactive)
    (async-shell-command "killall -9 rails ruby spring bundle; echo 'Ruby Instances Killed!'" "*Ruby Kill Output*") ))

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
                   (group . 1)
                   (modes . '(ruby-mode))))
  (add-to-list 'align-rules-list
                 '(ruby-hash-values
                   (regexp . ".to\\(\\s-*\\)")
                   (group . 1)
                   (modes . '(ruby-mode)))))

(use-package imenu-list
  :straight t)

(use-package robe
  :straight t
  :hook
  (ruby-mode . robe-mode)
  (ruby-ts-mode . robe-mode))

(use-package inf-ruby
  :straight t
  :config
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (eval-after-load 'company
  '(push 'company-robe company-backends)))

(use-package rubocop
  :straight t)

(use-package bundler
  :straight t)

(use-package rspec-mode
  :straight t
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup)
  (define-key rspec-mode-map (kbd "C-c . v") 'rspec-feature-verify-file)
  (define-key rspec-mode-map (kbd "C-c . s") 'rspec-feature-verify-single)
  (setq compilation-scroll-output t)
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

(use-package chatgpt-shell
  :straight (:host github
                   :repo "xenodium/chatgpt-shell"
                   :branch "main")
  :config
  (setq chatgpt-shell-openai-key "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"))

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
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package docker-compose-mode
  :straight t)

(use-package docker
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package plantuml-mode
  :straight t)


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
  (setenv "RUBYOPT" "-W0"))

(if window-system
    (progn
      (add-hook 'after-init-hook 'set-ruby-no-opt)
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

(global-set-key [escape] 'kos/keyboard-quit)

(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)
(global-set-key (kbd "C-x 3") 'split-and-follow-verticaly)
