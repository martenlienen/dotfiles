(require 'flycheck)

(add-hook 'after-init-hook 'global-flycheck-mode)
(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc)))
