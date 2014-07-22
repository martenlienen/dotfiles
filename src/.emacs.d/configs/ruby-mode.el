(cqql/after-load 'ruby-mode
  (setf ruby-insert-encoding-magic-comment nil)

  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'ruby-electric-mode)
  (add-hook 'ruby-mode-hook 'yard-mode)
  (add-hook 'ruby-mode-hook 'eldoc-mode)
  (add-hook 'ruby-mode-hook 'subword-mode))

(cqql/add-auto-mode 'ruby-mode
                    "Rakefile\\'"
                    "Capfile\\'"
                    "Vagrantfile\\'"
                    "Berksfile\\'"
                    ".gemspec\\'"
                    ".json_builder\\'"
                    "Gemfile\\'")
