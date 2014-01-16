;; Disable splash screen
(setq inhibit-startup-message t)

;; Disable visual stuff
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'color-theme)
(load "wombat.el")
(load-theme 'brin t)

;; Color nested parens rainbow-like
(global-rainbow-delimiters-mode)

;; Disable menu bar
(menu-bar-mode -1)

;; Show line numbers
(setq linum-format "%3d ")
(global-linum-mode t)

;; y and n instead of yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;; Create new files and buffers without confirmation
(setq confirm-nonexistent-file-or-buffer nil)
(setq ido-create-new-buffer 'always)

;; Only GC every 20 MB
(setq gc-cons-threshold 20000000)

;; Indent with 2 spaces
(setq-default indent-tabs-mode nil
              tab-width 2
              js-indent-level 2
              css-indent-offset 2
              sh-indentation 2)

;; Disable backups and autosaves
(setq backup-inhibited t)
(setq auto-save-default nil)

(add-hook 'before-save-hook 'cqql/trim-whitespace)

(provide 'globals)
