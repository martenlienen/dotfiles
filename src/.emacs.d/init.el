(push "~/.emacs.d" load-path)

;; Load/install packages
(require 'packages)

;; Set global/emacs-wide settings
(require 'globals)

;; Global requires
(require 'dash)
(require 's)

(require 'cqql-utils)

;; Load config files
(cqql/load-files "~/.emacs.d/configs")

(require 'bindings)
