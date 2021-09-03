;;; cqql-deadgrep.el --- Shortcuts for deadgrep

;;; Commentary:

;;; Code:

(defun cqql-deadgrep-search-term ()
  "Change the search term in deadgrep."
  (interactive)
  ;; Ignores the button so we can just call it
  (deadgrep--search-term nil))

(defun cqql-deadgrep-here ()
  "Run a deadgrep search in the current buffer's directory."
  (interactive)
  (let* ((root default-directory)
         (deadgrep-project-root-function (lambda () root)))
    (call-interactively #'deadgrep)))

(provide 'cqql-deadgrep)
;;; cqql-deadgrep.el ends here
