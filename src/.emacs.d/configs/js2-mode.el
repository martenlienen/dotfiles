(require 'js2-mode)
(require 'js2-refactor)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq-default js2-basic-offset 2)
(setf js2-highlight-level 3
      js2-include-node-externs t)

(js2r-add-keybindings-with-prefix "C-c r")

(add-hook 'js2-mode-hook 'subword-mode)
