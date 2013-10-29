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

    ; Libraries
    dash
    s

    ; Autocomplete
    icicles
    projectile
    flx-ido
    auto-complete
    ac-nrepl
    ac-math
    company

    ; Snippets
    yasnippet

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
    rspec-mode

    ; LaTeX
    auctex))

(require 'cl-lib)

(defun cqql/missing-packages (packages)
 (cl-reduce
  (lambda (acc package)
    (if (package-installed-p package)
      acc
      (cons package acc)))
  packages
  :initial-value '()))

(let ((missing (cqql/missing-packages cqql/packages)))
  (when missing
    (package-refresh-contents)
    (mapc 'package-install missing)))

(require 'dash)
(require 's)


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


;; Backups

; Disable backups and autosaves
(setq backup-inhibited t)
(setq auto-save-default nil)


;; Enhancing emacs

(require 'auto-complete)
(projectile-global-mode)
(icy-mode t)
(key-chord-mode t)


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


;; Load config files

(defun cqql/load-files (directory)
  (mapc
   (lambda (file)
     (load file))
   (-filter (lambda (file) (not (s-ends-with? "." file))) (directory-files directory t))))

(cqql/load-files "~/.emacs.d/configs")
