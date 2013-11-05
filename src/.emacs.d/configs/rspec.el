(require 'rspec-mode)

(defconfig rspec
  :settings ((rspec-use-rake-when-possible nil))
  :evil-keys ((normal "SPC f" 'rspec-verify-single)
              (normal "SPC r r" 'rspec-rerun)
              (normal "SPC r f" 'rspec-verify)
              (normal "SPC r g" 'rspec-verify-all)))
