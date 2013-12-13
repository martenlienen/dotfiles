(push "~/.emacs.d" load-path)

;; Load/install packages
(require 'packages)

;; Set global/emacs-wide settings
(require 'globals)

;; Global requires
(require 'dash)
(require 's)

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

(require 'bindings)
