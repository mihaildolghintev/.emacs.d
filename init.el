(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(require 'package)
(package-initialize)
;; (package-refresh-contents)




(setq my-packages
      '(
        cider
	lsp-mode
	clj-refactor
        projectile
        clojure-mode
        expand-region
        magit
        markdown-mode
        paredit
	company
	evil
	undo-fu
	evil-leader
	zenburn-theme
	ivy
	sketch-themes
	flycheck
	flycheck-clj-kondo
	wrap-region
	counsel
        ))


(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; credentials
(setq user-full-name    "Misha Dolghintev")
(setq user-mail-address "yadolghintev@gmail.com")

;; theme
(load-theme 'zenburn t)

 ;; don't want ESC as a modifier
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-o") 'counsel-imenu)



(wrap-region-mode t)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; UI
(tooltip-mode      -1)
(menu-bar-mode     -1)
(tool-bar-mode     -1)
(blink-cursor-mode -1)
(setq use-dialog-box nil)
(setq redisplay-dont-pause t)
(setq ring-bell-function 'ignore)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq frame-title-format "Emacs %b")


;;cider
(require 'cider)
(setq cider-overlays-use-font-lock t)
(setq cider-save-file-on-load nil)
(setq cider-save-file-on-load t)
(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
(setq cider-auto-select-error-buffer nil)

(require 'clj-refactor)
(require 'flycheck-clj-kondo)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)


;; pairs
(electric-pair-mode t)
;; (electric-indent-mode t)
(show-paren-mode 1)
(setq show-paren-style 'parenthesis) ;; 'expression

;; fringe
(require 'fringe)
(fringe-mode '(8 . 0))
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; line wrapping
(setq word-wrap t)
(setq truncate-lines t)
(setq truncate-partial-width-windows t)
(setq global-visual-line-mode nil)


;; no backups
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auto-save-list-file-name nil)

;; encodings
(setq default-buffer-file-coding-system 'utf-8)
(setq-default coding-system-for-read 'utf-8)
(setq file-name-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8-unix)

;; Line numbers
(line-number-mode   t)
(global-linum-mode  t)
(column-number-mode t)
(setq linum-format " %d")

;; syntax
(require 'font-lock)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(set-face-attribute 'default nil :family "Cascadia Code" :weight 'medium :height 105)


(add-hook 'emacs-lisp-mode-hook       #'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
(add-hook 'ielm-mode-hook             #'paredit-mode)
(add-hook 'lisp-mode-hook             #'paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'paredit-mode)
(add-hook 'scheme-mode-hook           #'paredit-mode)

;; clojure
(add-hook 'cider-mode-hook (lambda () (show-paren-mode 1)))
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)
(add-hook 'clojure-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'hl-todos)
(setq cider-font-lock-dynamically nil)
(setq cider-repl-use-pretty-printing t)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-signature-auto-activate nil 
      ; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )

;; clojurescript
(add-hook 'clojurescript-mode #'paredit-mode)


;;company
(global-company-mode)
(setq company-idle-delay 0)
(global-set-key (kbd "M-i") #'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
(define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)



;;projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)
(setq projectile-track-known-projects-automatically t)
;;ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)


(global-set-key (kbd "RET")   'newline-and-indent)

;; (evil-leader/set-key "ff" 'counsel-find-file)
;; (evil-leader/set-key "wo" 'other-window)
;; (evil-leader/set-key "wh" 'split-window-horizontally)
;; (evil-leader/set-key "wv" 'split-window-vertically)
;; (evil-leader/set-key "wt" 'delete-other-windows)
;; (evil-leader/set-key "ka" 'paredit-splice-sexp-killing-backward)
;; (evil-leader/set-key "mi" 'er/mark-inside-pairs)
;; (evil-leader/set-key "mo" 'er/mark-outside-pairs)


;; (evil-leader/set-key "ra" 'cljr-add-project-dependency)
;; (evil-leader/set-key "rr" 'cljr-add-require-to-ns)


;; (evil-leader/set-key "pp" 'projectile-switch-project)
;; (evil-leader/set-key "pf" 'projectile-find-file)

;; (evil-leader/set-key "ec" 'cider-eval-last-sexp)
;; (evil-leader/set-key "ed" 'cider-eval-defun-at-point)
;; (evil-leader/set-key "eb" 'cider-eval-buffer)



;; (evil-define-key 'normal 'global (kbd "(") 'paredit-wrap-round)
;; (evil-define-key 'normal 'global (kbd "[") 'paredit-wrap-square)
;; (evil-define-key 'normal 'global (kbd "{") 'paredit-wrap-curly)

;; (evil-define-key 'visual 'global (kbd "(") 'paredit-wrap-round)
;; (evil-define-key 'visual 'global (kbd "[") 'paredit-wrap-square)
;; (evil-define-key 'visual 'global (kbd "{") 'paredit-wrap-curly)

;; (require 'expand-region)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel ivy zenburn-theme projectile paredit markdown-mode magit expand-region company cider)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
