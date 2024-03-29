;;; cqql.el --- My custom functions

;;; Commentary:

;;; Code:

(require 'cl-lib)

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
                           (goto-char (region-beginning)))
                       (beginning-of-line)
                       (point)))
         (text-end (save-excursion
                     (if region?
                         (goto-char (region-end)))
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
      (goto-char (+ insert-start point-offset))
      (when (and region? mark?)
        (set-mark (+ insert-start mark-offset))
        (setq deactivate-mark nil)))))

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
                           (goto-char (region-beginning)))
                       (beginning-of-line)
                       (point)))
         (text-end (save-excursion
                     (if region?
                         (goto-char (region-end)))
                     (end-of-line)
                     (point)))
         (mark? (mark))
         (mark-offset (when mark? (- mark? text-start)))
         (text (buffer-substring text-start text-end))
         (new-pos (+ 1 pos (* times (length text)))))
    (if region?
        (goto-char text-end))
    (dotimes (i times)
      (end-of-line)
      (insert "\n")
      (insert text))
    (goto-char new-pos)
    (setq deactivate-mark nil)
    (when (and region? mark?)
      (set-mark (+ 1 mark? (* times (length text))))
      (setq deactivate-mark nil))))

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
      (goto-char pos))))

(defun cqql-c-append-semicolon ()
  "Insert semicolon at the end of the line."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (insert ";")))

(defvar cqql-no-trimming-modes '()
  "A list of modes, that should not be whitespace-trimmed.")

(defun cqql-trim-whitespace ()
  (when (not (seq-contains-p cqql-no-trimming-modes major-mode))
    (delete-trailing-whitespace)))

(provide 'cqql)
;;; cqql.el ends here
