(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))
