(require 'ruby-additional)
(require 'ruby-electric)

(defconfig ruby
  :hooks (robe-mode ruby-electric-mode)
  :files ("Gemfile" "\\.gemspec\\'"))
