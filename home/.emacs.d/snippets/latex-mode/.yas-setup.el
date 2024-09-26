;;; yas-setup --- Setup for latex snippets

;;; Commentary:

;;; Code:

(defun isnip-latex-to-label (text)
  (s-dashed-words text))

(defun isnip-latex-last-macro (&optional n)
  "Find the name of the N th last macro."
  (save-excursion
    (cl-loop repeat (or n 1) do (search-backward "\\"))
    (forward-char)
    (word-at-point)))

(defun isnip-latex-label-prefix ()
  (or
   (let ((environment (LaTeX-current-environment)))
     (pcase environment
       ((or "equation" "equation*" "align" "align*") "eq:")
       ((or "theorem" "proposition" "lemma" "corollary") "thm:")
       ((or "figure") "fig:")))
   (let ((macro (isnip-latex-last-macro 2)))
     (message macro)
     (pcase macro
       ((or "section" "subsection" "subsubsection") "sec:")))))
