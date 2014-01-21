;; Load/install packages
(require 'cask "~/.cask/cask.el")
(cask-initialize)

(push "~/.emacs.d" load-path)

(require 'cqql-utils)

;; Set global/emacs-wide settings
(require 'globals)

;; Global requires
(require 'dash)
(require 's)

;; Load config files
(cqql/load-files "~/.emacs.d/configs")

(require 'bindings)
