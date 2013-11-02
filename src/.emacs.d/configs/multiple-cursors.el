(defconfig multiple-cursors
  :global-evil-keys ((insert "C->" 'mc/mark-next-like-this)
                     (insert "C-M->" 'mc/skip-to-next-like-this)
                     (insert "C-<" 'mc/unmark-next-like-this)
                     (insert "M-n" 'mc/mark-all-like-this)
                     (insert "C-M-n" 'mc/edit-lines)))
