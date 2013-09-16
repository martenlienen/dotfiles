(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages
  '(clojure-mode
    clojure-test-mode
    nrepl
    paredit
    rainbow-delimiters
    color-theme-solarized
    markdown-mode))

(defun ensure-package-installed (package)
  (when (not (package-installed-p package))
    (package-install package)))

(when (null package-archive-contents)
	(package-refresh-contents))

(mapc 'ensure-package-installed my-packages)

;; UI

(load-theme 'solarized-dark t)

; Color nested parens rainbow-like
(global-rainbow-delimiters-mode)

; Disable menu bar
(menu-bar-mode -1)


;; Backups

; Disable backups and autosaves
(setq backup-inhibited t)
(setq auto-save-default nil)


;; Editing

; Automatically indent
(electric-indent-mode 1)

(autoload 'enable-paredit-mode "paredit" "Slurp those parens" t)
(defvar my-lisp-mode-hooks
  '(emacs-lisp-mode-hook
    clojure-mode-hook
    nrepl-mode-hook))
(mapc (lambda (hook) (add-hook hook #'enable-paredit-mode)) my-lisp-mode-hooks)
(mapc (lambda (hook) (add-hook hook #'turn-on-eldoc-mode)) my-lisp-mode-hooks)
