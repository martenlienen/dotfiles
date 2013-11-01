(require 'magit)

(defun cqql/magit-commit-all ()
  (interactive)
  (magit-commit-internal "commit" '("--all")))

(defconfig magit
  :global-evil-keys ((normal "SPC g g" 'magit-status)
                     (normal "SPC g c" 'cqql/magit-commit-all)))
