(require 'ruby-mode)

(setf ruby-deep-indent-paren nil
      ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-mode-hook 'robe-mode)

(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
