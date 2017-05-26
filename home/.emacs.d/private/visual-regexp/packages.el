;;; packages.el --- visual-regexp layer packages file for Spacemacs.

;;; Commentary:

;;; Code:

(defconst visual-regexp-packages
  '(visual-regexp))

(defun visual-regexp/init-visual-regexp ()
  (use-package visual-regexp
    :bind (("M-%" . vr/query-replace))))

;;; packages.el ends here
