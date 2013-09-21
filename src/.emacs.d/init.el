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

    ; Markdown
    markdown-mode

    ; Lisp stuff
    clojure-mode
    clojure-test-mode
    nrepl
    paredit
    rainbow-delimiters))

(defun ensure-package-installed (package)
  (when (not (package-installed-p package))
    (package-install package)))

(when (null package-archive-contents)
	(package-refresh-contents))

(mapc 'ensure-package-installed cqql/packages)

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

(icicle-mode)
(evil-mode)
(key-chord-mode t)


;; Evil

; Remap jk to ESC
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

; , is much easier to type when your fingers are on the motion keys
(define-key evil-normal-state-map (kbd ",") 'evil-ex)

; Ace jump
(define-key evil-normal-state-map (kbd "SPC w") 'ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "SPC h") 'ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "SPC j") 'ace-jump-line-mode)

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

(define-key evil-normal-state-map (kbd "SPC g s") 'magit-status)
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
(mapc (lambda (hook) (add-hook hook 'cqql/indent-on-enter)) cqql/lisp-mode-hooks)
