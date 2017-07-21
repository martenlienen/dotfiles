;;; yas-setup --- Setup for js snippets

;;; Commentary:

;;; Code:

(defun isnip-js-end-of-object? ()
  "Is point at the end of an object declaration?"
  (save-excursion
    (re-search-forward "[^[:space:][:cntrl:]]" nil t)
    (looking-back "}")))
