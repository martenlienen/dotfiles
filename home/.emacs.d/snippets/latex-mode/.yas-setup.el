;;; yas-setup --- Setup for latex snippets

;;; Commentary:

;;; Code:

(defun isnip-latex-to-label (text)
  (s-dashed-words text))
