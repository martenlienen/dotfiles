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
    smartparens

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
    cider
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


;; Global requires

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

;; Macros
(defun cqql/body-to-hash (body)
  (let ((parts (-partition 2 body))
        (settings (make-hash-table)))
    (-map
     (lambda (tuple) (puthash (car tuple) (cadr tuple) settings))
     parts)
    settings))

(defmacro defconfig (mode &rest body)
  "A macro to configure modes"
  (let* ((mode-symbol (intern (concat (symbol-name mode) "-mode")))
         (evil-mode-name (intern (concat "evil-" (symbol-name mode) "-mode")))
         (evil-keymap (intern (concat (symbol-name evil-mode-name) "-map")))
         (evil-hook-name (intern (concat (symbol-name evil-mode-name) "-hook")))
         (hook-name (intern (concat (symbol-name mode) "-mode-hook")))
         (hash (cqql/body-to-hash body))
         (settings (gethash :settings hash))
         (evil-keys (gethash :evil-keys hash))
         (global-evil-keys (gethash :global-evil-keys hash))
         (hooks (gethash :hooks hash))
         (files (gethash :files hash)))
    `(progn
       ,@(when settings
           (-map
            (lambda (setting) `(setq ,(car setting) ,(cadr setting)))
            settings))
       ,(when evil-keys
          `(progn
             (define-minor-mode ,evil-mode-name
               ,(concat "Evil mode bindings for " (symbol-name mode))
               :keymap (make-sparse-keymap))
             ,@(-map
                (lambda (key) `(evil-define-key ',(car key) ,evil-keymap (kbd ,(cadr key)) ',(caddr key)))
                evil-keys)
             (add-hook ',hook-name ',evil-mode-name)))
       ,(when global-evil-keys
          `(eval-after-load "evil"
             '(progn 
                ,@(-map
                   (lambda (key) `(define-key ,(intern (concat "evil-" (symbol-name (car key)) "-state-map")) (kbd ,(cadr key)) ,(caddr key)))
                   global-evil-keys))))
       ,@(when hooks
           (-map
            (lambda (hook) `(add-hook ',hook-name ',hook))
            hooks))
       ,@(when files
           (-map
            (lambda (regex) `(add-to-list 'auto-mode-alist '(,regex . ,mode-symbol)))
            files)))))

;; Load config files

(defun cqql/load-files (directory)
  (mapc
   (lambda (file)
     (load file))
   (-filter (lambda (file) (not (s-ends-with? "." file))) (directory-files directory t))))

(cqql/load-files "~/.emacs.d/configs")
