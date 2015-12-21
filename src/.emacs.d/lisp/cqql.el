;;; cqql --- My custom functions

;;; Commentary:

;;; Code:

(require 'dash)

(defun cqql-go-to-beginning-of-line-dwim ()
  "Toggle point between beginning of line and first non-whitespace character."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (when (= prev-pos (point))
      (move-beginning-of-line nil))))

(defun cqql-duplicate-text (times)
  "Duplicate the current line or region TIMES times.

If the region is active, it duplicates from the start of the
first line of the region to the end of the last."
  (interactive "p")
  (let* ((region? (use-region-p))
         (pos (point))
         (text-start (save-excursion
                       (if region?
                           (setf (point) (region-beginning)))
                       (beginning-of-line)
                       (point)))
         (text-end (save-excursion
                     (if region?
                         (setf (point) (region-end)))
                     (end-of-line)
                     (point)))
         (text (buffer-substring text-start text-end))
         (new-pos (+ pos (* times (length text)) 1)))
    (if region?
        (setf (point) text-end))
    (dotimes (i times)
      (end-of-line)
      (insert "\n")
      (insert text))
    (setf (point) new-pos)))

(defun cqql-open-line ()
  "Create a new line below and put point into it."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun cqql-open-line-above ()
  "Create a new line above point and move point into it."
  (interactive)
  (move-beginning-of-line nil)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun cqql-kill-line ()
  "Kill the current line."
  (interactive)
  (let ((pos (point)))
    (move-beginning-of-line nil)
    (kill-line 1)
    (move-end-of-line nil)
    (when (< pos (point))
      (setf (point) pos))))

(defun cqql-c-append-semicolon ()
  "Insert semicolon at the end of the line."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (insert ";")))

(defun cqql-latex-append-line-break ()
  "Insert the \\\\ macro at the end of the line."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (insert "\\\\")))

(defun cqql-exercise-headers (structure)
  "Create a STRUCTURE of headers for exercise sheets."
  (interactive "xStructure: ")
  (cl-loop for node in structure
           do
           (if (listp node)
               (cl-loop for subnode in node
                        do (insert (format "\\subsection*{Part %s)}\n\n"
                                           subnode)))
             (insert (format "\\section*{Exercise %s}\n\n" node)))))

(defvar cqql-no-trimming-modes '()
  "A list of modes, that should not be whitespace-trimmed.")

(defun cqql-trim-whitespace ()
  (when (not (-contains? cqql-no-trimming-modes major-mode))
    (delete-trailing-whitespace)))

(defmacro cqql-after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defmacro cqql-define-keys (keymap &rest bindings)
  (declare (indent defun))
  `(progn
     ,@(-map
        (lambda (binding) `(define-key ,keymap (kbd ,(car binding)) ,(cadr binding)))
        bindings)))

(defmacro cqql-define-global-keys (&rest bindings)
  (declare (indent defun))
  `(progn
     ,@(-map
        (lambda (binding) `(global-set-key (kbd ,(car binding)) ,(cadr binding)))
        bindings)))

(provide 'cqql)
;;; cqql.el ends here
