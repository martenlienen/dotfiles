;; -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Temporarily disable the GC during initialization
(defun ml-restore-gc-threshold ()
  "Reset the GC threshold to a reasonable value."
  (setq gc-cons-threshold (* 16 1024 1024)))
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook #'ml-restore-gc-threshold)

;; Temporarily disable special file name handling to improve library loading speed
(defvar ml-default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(defun ml-restore-file-name-handler-alist ()
  "Restore the file-name-handler alist."
  (setq file-name-handler-alist ml-default-file-name-handler-alist))

(add-hook 'emacs-startup-hook #'ml-restore-file-name-handler-alist)

;; Disable splash screen
(setq inhibit-startup-message t)
