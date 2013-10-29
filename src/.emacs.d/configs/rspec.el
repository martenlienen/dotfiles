(require 'rspec-mode)

(setq rspec-use-rake-when-possible nil)
(evil-define-key 'normal rspec-mode-keymap (kbd "SPC f") 'rspec-verify-single)
(evil-define-key 'normal rspec-mode-keymap (kbd "SPC r r") 'rspec-rerun)
(evil-define-key 'normal rspec-mode-keymap (kbd "SPC r f") 'rspec-verify)
(evil-define-key 'normal rspec-mode-keymap (kbd "SPC r g") 'rspec-verify-all)
