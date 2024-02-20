;;; cqql-deadgrep.el --- Shortcuts for deadgrep

;;; Commentary:

;;; Code:

(require 'deadgrep)

(defun cqql-deadgrep-search-term ()
  "Change the search term in deadgrep."
  (interactive)
  ;; Ignores the button so we can just call it
  (deadgrep--search-term nil))

(defun cqql-deadgrep-directory ()
  "Change the directory in deadgrep."
  (interactive)
  ;; Ignores the button so we can just call it
  (deadgrep--directory nil))

(defun cqql-deadgrep-file-type (type)
  "Set the file type to TYPE."
  (let ((button (make-button 0 0 :type 'deadgrep-file-type 'file-type type)))
    (deadgrep--file-type button)))

(defun cqql-deadgrep-file-type-all ()
  "Search all file types in deadgrep."
  (interactive)
  (cqql-deadgrep-file-type 'all))

(defun cqql-deadgrep-file-type-type ()
  "Search certain file types in deadgrep."
  (interactive)
  (cqql-deadgrep-file-type 'type))

(defun cqql-deadgrep-file-type-glob ()
  "Select file types by glob in deadgrep."
  (interactive)
  (cqql-deadgrep-file-type 'glob))

(defun cqql-deadgrep-here ()
  "Run a deadgrep search in the current buffer's directory."
  (interactive)
  (let* ((root default-directory)
         (deadgrep-project-root-function (lambda () root)))
    (call-interactively #'deadgrep)))

(provide 'cqql-deadgrep)
;;; cqql-deadgrep.el ends here
