;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Temporarily disable the GC during initialization
(defun ml-restore-gc-threshold ()
  "Reset the GC threshold to a reasonable value."
  (setq gc-cons-threshold (* 16 1024 1024)))
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook #'ml-restore-gc-threshold)

;; Disable splash screen
(setq inhibit-startup-message t)
