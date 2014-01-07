(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
