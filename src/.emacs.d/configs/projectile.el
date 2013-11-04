(projectile-global-mode)

(defconfig projectile
  :global-evil-keys ((normal "SPC t" 'projectile-find-file)
                     (normal "C-x C-f" 'projectile-find-file)))
