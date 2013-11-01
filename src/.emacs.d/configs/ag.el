(require 'ag)

(add-to-list 'ag-arguments "--hidden")

(defconfig ag
  :global-evil-keys ((normal "SPC \\" 'ag-project)))
