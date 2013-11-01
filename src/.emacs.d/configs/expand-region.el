(require 'expand-region)

(defconfig expand-region
  :global-evil-keys ((insert "C-m" 'er/expand-region)
                     (insert "C-M-m" 'er/contract-region)))
