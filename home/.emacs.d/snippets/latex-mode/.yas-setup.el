;;; yas-setup --- Setup for latex snippets

;;; Commentary:

;;; Code:

(defun isnip-latex-to-label (text)
  (s-dashed-words text))

(defun isnip-latex-label-prefix ()
  (let ((environment (LaTeX-current-environment)))
    (pcase environment
      ((or "equation" "equation*" "align" "align*") "eq:")
      ((or "figure") "fig:"))))
