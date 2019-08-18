;;; cqql-deadgrep.el --- Shortcuts for deadgrep

;;; Commentary:

;;; Code:

(defun cqql-deadgrep-search-term ()
  "Change the search term in deadgrep."
  (interactive)
  ;; Ignores the button so we can just call it
  (deadgrep--search-term nil))

(provide 'cqql-deadgrep)
;;; cqql-deadgrep.el ends here
