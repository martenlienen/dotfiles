;;; packages.el --- rst layer packages file for Spacemacs.

;;; Commentary:

;;; Code:

(defconst rst-packages
  '(rst))

(defun rst/init-rst ()
  (use-package rst
    :bind (:map rst-mode-map ("<C-right>" . rst-underline))))

;;; packages.el ends here
