;; Disable splash screen
(setq inhibit-startup-message t)

(setf ring-bell-function 'ignore)

;; Color theme
(load-theme 'material t)

(setq-default cursor-type 'bar)

;; Use dejavu to display greek symbols (Source Code Pro has really weird greek
;; characters)
(set-fontset-font "fontset-default" 'greek "DejaVu Sans Mono")

;; Configure the *scratch* buffer
(setf initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode)

;; Enable X clipboards
(setf x-select-enable-clipboard t
      x-select-enable-primary t)

;; Put more information into the frame title
(setf frame-title-format '("" invocation-name ": %b - " mode-name))

;; Enable all commands
(mapatoms (lambda (s) (when (get s 'disabled) (put s 'disabled nil))))

;; Show column numbers
(column-number-mode t)

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
              sh-indentation 2
              web-mode-markup-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-attr-indent-offset 2)

;; Line breaks at 80 characters
(setq-default fill-column 80)

;; Disable backups and autosaves
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Yank at point instead of position of cursor
(setf mouse-yank-at-point t)

(when (memq window-system '(x))
  (exec-path-from-shell-initialize))

;; Disable tramp and GPG keys for performance
(setq file-name-handler-alist nil)

(provide 'globals)
