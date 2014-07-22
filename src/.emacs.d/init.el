;; Put this at the top, so that themes will be treated as safe, when they are
;; loaded
(custom-set-variables)

;; Load/install packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(push "~/.emacs.d/lisp" load-path)

(require 'cqql-utils)

;; Set global/emacs-wide settings
(require 'globals)

;; Global requires
(require 'dash)
(require 's)

;; Load config files
(cqql/load-files "~/.emacs.d/configs")

(require 'bindings)
