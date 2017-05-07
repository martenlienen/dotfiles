;;; cqql --- My custom functions

;;; Commentary:

;;; Code:

(defun cqql-go-to-beginning-of-line-dwim ()
  "Toggle point between beginning of line and first non-whitespace character."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (when (= prev-pos (point))
      (move-beginning-of-line nil))))

(defun cqql-move-text (lines)
  "Move the current line or region LINES lines."
  (interactive "p")
  (let* ((region? (use-region-p))
         (column (current-column))
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
         (mark? (mark))
         (mark-offset (when mark? (- mark? text-start)))
         (point-offset (- (point) text-start))
         (text (if (= text-end (point-max))
                   (concat (delete-and-extract-region text-start text-end)
                           "\n")
                 (delete-and-extract-region text-start (1+ text-end)))))
    (forward-line lines)
    (let ((insert-start (point)))
      (insert text)
      (setf (point) (+ insert-start point-offset))
      (when (and region? mark?)
        (setf (mark) (+ insert-start mark-offset)
              deactivate-mark nil)))))

(defun cqql-move-text-up (lines)
  "Move the current line or region LINES lines up."
  (interactive "p")
  (cqql-move-text (- lines)))

(defun cqql-move-text-down (lines)
  "Move the current line or region LINES lines down."
  (interactive "p")
  (cqql-move-text lines))

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
         (mark? (mark))
         (mark-offset (when mark? (- mark? text-start)))
         (text (buffer-substring text-start text-end))
         (new-pos (+ 1 pos (* times (length text)))))
    (if region?
        (setf (point) text-end))
    (dotimes (i times)
      (end-of-line)
      (insert "\n")
      (insert text))
    (setf (point) new-pos
          deactivate-mark nil)
    (when (and region? mark?)
      (setf (mark) (+ 1 mark? (* times (length text)))
            deactivate-mark nil))))

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
  (when (not (seq-contains cqql-no-trimming-modes major-mode))
    (delete-trailing-whitespace)))

(defmacro cqql-after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defmacro cqql-define-keys (keymap &rest bindings)
  (declare (indent defun))
  `(progn
     ,@(seq-map
        (lambda (binding) `(define-key ,keymap (kbd ,(car binding)) ,(cadr binding)))
        bindings)))

(defmacro cqql-define-global-keys (&rest bindings)
  (declare (indent defun))
  `(progn
     ,@(seq-map
        (lambda (binding) `(global-set-key (kbd ,(car binding)) ,(cadr binding)))
        bindings)))

(provide 'cqql)
;;; cqql.el ends here
