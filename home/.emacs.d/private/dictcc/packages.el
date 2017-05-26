;;; packages.el --- dictcc layer packages file for Spacemacs.

;;; Commentary:

;;; Code:

(defconst dictcc-packages
  '(dictcc))

(defun dictcc/init-dictcc ()
  (use-package dictcc
    :bind ("C-c d" . dictcc)))

;;; packages.el ends here
