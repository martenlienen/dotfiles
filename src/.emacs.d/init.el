(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar my-packages
  '(clojure-mode
    color-theme-solarized))

(defun ensure-package-installed (package)
  (when (not (package-installed-p package))
    (package-install package)))

(mapc 'ensure-package-installed my-packages)

;; UI

(load-theme 'solarized-dark t)

; Disable menu bar
(menu-bar-mode -1)


;; Backups

; Disable backups and autosaves
(setq backup-inhibited t)
(setq auto-save-default nil)
