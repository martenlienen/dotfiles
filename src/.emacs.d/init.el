; Disable splash screen
(setq inhibit-startup-message t)

; y and n instead of yes and no
(fset 'yes-or-no-p 'y-or-n-p)

; Create new files and buffers without confirmation
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

; Disable visual stuff
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

; Only GC every 20 MB
(setq gc-cons-threshold 20000000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(setq package-enable-at-startup nil)

(defvar cqql/packages
  '(                                    ; Editing
    evil
    ace-jump-mode
    key-chord
    multiple-cursors
    smartparens
    expand-region

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
    ruby-additional
    ruby-electric
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

(require 'color-theme)
(load "~/.emacs.d/wombat.el")
(load-theme 'wombat t)

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
                (lambda (key) `(evil-define-key ',(car key) ,evil-keymap (kbd ,(cadr key)) ,(caddr key)))
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

(defun cqql/go-to-beginning-of-line-dwim ()
  (interactive)
  "Toggle point between beginning of line and first non-whitespace character"
  (let ((prev-pos (point)))
    (back-to-indentation)
    (when (= prev-pos (point))
      (move-beginning-of-line nil))))

(defun cqql/duplicate-line ()
  (interactive)
  "Duplicate the current line and move point down"
  (let ((pos (point)))
    (move-beginning-of-line nil)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line)
    (yank)
    (goto-char pos)
    (next-line)))

(defun cqql/magit-commit-all ()
  (interactive)
  (magit-commit-internal "commit" '("--all")))

(defmacro cqql/define-keys (keymap &rest bindings)
  `(progn
     ,@(-map
        (lambda (binding) `(define-key ,keymap (kbd ,(car binding)) ,(cadr binding)))
        bindings)))

(defmacro cqql/define-global-keys (&rest bindings)
  `(progn
     ,@(-map
        (lambda (binding) `(global-set-key (kbd ,(car binding)) ,(cadr binding)))
        bindings)))

(defmacro cqql/define-global-evil-keys (mode &rest bindings)
  (let ((keymap (intern (format "evil-%s-state-map" mode))))
    `(cqql/define-keys ,keymap ,@bindings)))

(cqql/define-global-keys
 ("C-a" 'cqql/go-to-beginning-of-line-dwim)
 ("M-D" 'cqql/duplicate-line)
 ("M-^" 'evil-mode)
 ("C->" 'mc/mark-next-like-this)
 ("C-M->" 'mc/skip-to-next-like-this)
 ("C-<" 'mc/unmark-next-like-this)
 ("M-n" 'mc/mark-all-like-this)
 ("C-M-n" 'mc/edit-lines)
 ("M-m" 'er/expand-region)
 ("M-M" 'er/contract-region))

(cqql/define-global-evil-keys insert
 ("C-x C-f" 'projectile-find-file)
 ("C-M-f" 'sp-next-sexp)
 ("C-M-S-f" 'sp-forward-sexp)
 ("C-M-b" 'sp-backward-sexp)
 ("C-M-S-b" 'sp-previous-sexp)
 ("C-M-n" 'sp-down-sexp)
 ("C-M-S-n" 'sp-backward-down-sexp)
 (")" 'sp-up-sexp)
 ("C-M-S-p" 'sp-backward-up-sexp)
 ("C-M-a" 'sp-beginning-of-sexp)
 ("C-M-e" 'sp-end-of-sexp)
 ("C-M-k" 'sp-kill-sexp)
 ("C-M-S-k" 'sp-backward-kill-sexp)
 ("C-M-w" 'sp-copy-sexp)
 ("C-M-t" 'sp-transpose-sexp)
 ("C-M-h" 'sp-backward-slurp-sexp)
 ("C-M-S-h" 'sp-backward-barf-sexp)
 ("C-M-l" 'sp-forward-slurp-sexp)
 ("C-M-S-l" 'sp-forward-barf-sexp)
 ("C-M-j" 'sp-splice-sexp)
 ("C-M-S-j" 'sp-raise-sexp))

(cqql/define-global-evil-keys normal
 ("," 'evil-ex)
 ("SPC \\" 'ag-project)
 ("SPC g g" 'magit-status)
 ("SPC g c" 'cqql/magit-commit-all)
 ("SPC w" 'ace-jump-word-mode)
 ("SPC h" 'ace-jump-char-mode)
 ("SPC t" 'projectile-find-file)
 ("SPC f" 'rspec-verify-single)
 ("SPC r r" 'rspec-rerun)
 ("SPC r f" 'rspec-verify)
 ("SPC r g" 'rspec-verify-all))
