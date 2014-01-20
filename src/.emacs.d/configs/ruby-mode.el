(require 'ruby-mode)
(require 'ruby-electric)

(setf ruby-deep-indent-paren nil
      ruby-insert-encoding-magic-comment nil)

(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '(".gemspec\\'" . ruby-mode))
