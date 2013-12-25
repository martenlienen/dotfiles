(require 'enh-ruby-mode)

(setf enh-ruby-deep-indent-paren nil)

(add-hook 'enh-ruby-mode-hook 'robe-mode)

(add-to-list 'auto-mode-alist '("Gemfile\\'" . enh-ruby-mode))
