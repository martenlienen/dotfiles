;; Disable splash screen
(setq inhibit-startup-message t)

(setf ring-bell-function 'ignore)

;; Color theme
(load-theme 'smyx t)

(setq-default cursor-type 'bar)

;; Configure the *scratch* buffer
(setf initial-scratch-message ""
      initial-major-mode 'emacs-lisp-mode)

(setf initial-buffer-choice (lambda ()
                              (org-agenda-list)
                              (delete-other-windows)
                              org-agenda-buffer))

;; Enable X clipboards
(setf x-select-enable-clipboard t
      x-select-enable-primary t)

;; Show line numbers
(setq linum-format "%3d ")
(global-linum-mode t)

;; Put more information into the frame title
(setf frame-title-format '("" invocation-name ": %b - " mode-name))

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

(add-hook 'before-save-hook 'cqql/trim-whitespace)

(when (memq window-system '(x))
  (exec-path-from-shell-initialize))

(provide 'globals)
