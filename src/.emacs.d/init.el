; Disable splash screen
(setq inhibit-startup-message t)

; Only GC every 20 MB
(setq gc-cons-threshold 20000000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar cqql/packages
  '(; UI
    color-theme-solarized

    ; Editing
    evil
    ace-jump-mode
    key-chord
    multiple-cursors

    ; Git wrapper
    magit

    ; Searching
    ag

    ; Autocomplete
    icicles
    projectile
    flx-ido
    auto-complete
    ac-nrepl
    company

    ; Syntax checking
    flycheck

    ; Markdown
    markdown-mode

    ; Lisp stuff
    clojure-mode
    clojure-test-mode
    nrepl
    paredit
    rainbow-delimiters

    ; Ruby stuff
    robe
    company-inf-ruby
    rspec-mode))

(defun cqql/ensure-package-installed (package)
  (when (not (package-installed-p package))
    (package-install package)))

(when (null package-archive-contents)
  (package-refresh-contents))

(mapc 'cqql/ensure-package-installed cqql/packages)

;; UI

(load-theme 'solarized-dark t)

; Color nested parens rainbow-like
(global-rainbow-delimiters-mode)

; Disable menu bar
(menu-bar-mode -1)

; Show line numbers
(setq linum-format "%3d ")
(global-linum-mode t)


;; Editing

; Indent with 2 spaces
(setq-default indent-tabs-mode nil)
(setq tab-width 2)


;; Enhancing emacs

(require 'auto-complete)
(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
(projectile-global-mode)
(icy-mode t)
(evil-mode)
(key-chord-mode t)


;; ruby-mode

(add-hook 'ruby-mode-hook 'robe-mode)
(require 'rspec-mode)
(setq rspec-use-rake-when-possible nil)
(define-key evil-normal-state-map (kbd "SPC f") 'rspec-verify-single)
(define-key evil-normal-state-map (kbd "SPC r r") 'rspec-rerun)
(define-key evil-normal-state-map (kbd "SPC r f") 'rspec-verify)
(define-key evil-normal-state-map (kbd "SPC r g") 'rspec-verify-all)


;; company-mode

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-robe))


;; nrepl

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-mode-hook 'auto-complete-mode)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'nrepl-mode)


;; flycheck

(add-hook 'after-init-hook 'global-flycheck-mode)


;; Evil

; Toggle evil mode
(define-key evil-normal-state-map (kbd "M-^") 'evil-mode)
(global-set-key (kbd "M-^") 'evil-mode)

; Remap jk to ESC
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; Do not overload RET and SPC
(define-key evil-insert-state-map (kbd "RET") nil)
(define-key evil-insert-state-map (kbd "SPC") nil)

; , is much easier to type when your fingers are on the motion keys
(define-key evil-normal-state-map (kbd ",") 'evil-ex)

; Open files
(define-key evil-normal-state-map (kbd "SPC t") 'projectile-find-file)

; Ace jump
(define-key evil-normal-state-map (kbd "SPC w") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "SPC h") 'ace-jump-char-mode)

; Toggle comments
(define-key evil-visual-state-map (kbd "c") 'comment-or-uncomment-region)

; Searching with ag
(require 'ag)
(add-to-list 'ag-arguments "--hidden")
(define-key evil-normal-state-map (kbd "SPC \\") 'ag-project)

; Magit
(require 'magit)
(defun cqql/magit-commit-all ()
  (interactive)
  (magit-commit-internal "commit" '("--all")))

(define-key evil-normal-state-map (kbd "SPC g g") 'magit-status)
(define-key evil-normal-state-map (kbd "SPC g c") 'cqql/magit-commit-all)

; Paredit
(define-key evil-normal-state-map (kbd "M-l") 'paredit-forward-slurp-sexp)
(define-key evil-normal-state-map (kbd "M-h") 'paredit-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "M-k") 'paredit-split-sexp)
(define-key evil-normal-state-map (kbd "M-j") 'paredit-join-sexps)
(define-key evil-normal-state-map (kbd "M-n") 'paredit-splice-sexp)
(define-key evil-normal-state-map (kbd "M-m") 'paredit-raise-sexp)
(define-key evil-normal-state-map (kbd "M-[") 'paredit-wrap-square)
(define-key evil-normal-state-map (kbd "M-{") 'paredit-wrap-curly)
(define-key evil-normal-state-map (kbd "M-(") 'paredit-wrap-round)

; Window management
(define-key evil-normal-state-map (kbd "C-M-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-M-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-M-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-M-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "M-c") 'split-window-horizontally)
(define-key evil-normal-state-map (kbd "M-s") 'split-window-vertically)


;; Backups

; Disable backups and autosaves
(setq backup-inhibited t)
(setq auto-save-default nil)


;; Enable lisp mode hooks
(autoload 'enable-paredit-mode "paredit" "Slurp those parens" t)

(defun cqql/indent-on-enter ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(defvar cqql/lisp-mode-hooks
  '(emacs-lisp-mode-hook
    clojure-mode-hook
    nrepl-mode-hook))

(mapc (lambda (hook) (add-hook hook 'enable-paredit-mode)) cqql/lisp-mode-hooks)
(mapc (lambda (hook) (add-hook hook 'turn-on-eldoc-mode)) cqql/lisp-mode-hooks)
(mapc (lambda (hook) (add-hook hook 'cqql/indent-on-enter)) '(emacs-lisp-mode-hook clojure-mode-hook))
