(cqql/after-load 'ruby-mode
  (require 'rspec-mode)

  (setq rspec-use-rake-when-possible nil))
