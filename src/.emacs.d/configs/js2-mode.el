(cqql/after-load 'js2-mode
  (require 'js2-refactor)

  (setq-default js2-basic-offset 2)
  (setf js2-highlight-level 3
        js2-include-node-externs t)

  (js2r-add-keybindings-with-prefix "C-c r")

  (add-hook 'js2-mode-hook 'subword-mode))

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(cqql/add-auto-mode 'js2-mode "\\.js\\'" "\\.jsx\\'")
