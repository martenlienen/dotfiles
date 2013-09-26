; Disable splash screen
(setq inhibit-startup-message t)

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

    ; Git wrapper
    magit

    ; Searching
    ag

    ; Autocomplete
    icicles
    auto-complete
    ac-nrepl

    ; Syntax checking
    flycheck

    ; Markdown
    markdown-mode

    ; Lisp stuff
    clojure-mode
    clojure-test-mode
    nrepl
    paredit
    rainbow-delimiters))

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


;; Enhancing emacs

(require 'auto-complete)
(icicle-mode)
(global-auto-complete-mode)
(evil-mode)
(key-chord-mode t)


;; nrepl

(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'nrepl-mode)


;; flycheck

(add-hook 'after-init-hook 'global-flycheck-mode)


;; Evil

; Remap jk to ESC
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; Do not overload RET and SPC
(define-key evil-insert-state-map (kbd "RET") nil)
(define-key evil-insert-state-map (kbd "SPC") nil)

; , is much easier to type when your fingers are on the motion keys
(define-key evil-normal-state-map (kbd ",") 'evil-ex)

; Open files
(define-key evil-normal-state-map (kbd "SPC t") 'icicle-locate-file)

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
(define-key evil-normal-state-map (kbd "C-<LEFT>") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "C-<RIGHT>") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "C-<DOWN>") 'evil-window-decrease-height)
(define-key evil-normal-state-map (kbd "C-<UP>") 'evil-window-increase-height)


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
