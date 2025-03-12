;;; mlutils.el --- My custom functions

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defun ml/beginning-of-line-or-text ()
  "Toggle point between beginning of line and first non-whitespace character."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (when (= prev-pos (point))
      (move-beginning-of-line nil))))

(defun ml/beginning-of-line ()
  "Move to the beginning of the line.

Goes by visual line except with prefix argument where it always goes to
beginning of the logical line."
  (interactive)
  (if (bound-and-true-p visual-line-mode)
      (if current-prefix-arg
          (ml/beginning-of-line-or-text)
        (if (= (save-excursion (beginning-of-visual-line) (point))
               (save-excursion (beginning-of-line) (point)))
            ;; We are in the first line of a multi-line visual line
            (ml/beginning-of-line-or-text)
          (beginning-of-visual-line)))
    (ml/beginning-of-line-or-text)))

(defun ml/end-of-line ()
  "Go to the end of the line.

Goes by visual line except with prefix argument where it always goes to
the end of the logical line."
  (interactive)
  (if (bound-and-true-p visual-line-mode)
      (if current-prefix-arg
          (end-of-line)
        (end-of-visual-line))
    (end-of-line)))

(defun ml/move-text (lines)
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

(defun ml/move-text-up (lines)
  "Move the current line or region LINES lines up."
  (interactive "p")
  (ml/move-text (- lines)))

(defun ml/move-text-down (lines)
  "Move the current line or region LINES lines down."
  (interactive "p")
  (ml/move-text lines))

(defun ml/duplicate-text (times)
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

(defun ml/open-line ()
  "Create a new line below and put point into it."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun ml/open-line-above ()
  "Create a new line above point and move point into it."
  (interactive)
  (move-beginning-of-line nil)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun ml/kill-line ()
  "Kill the current line."
  (interactive)
  (let ((pos (point)))
    (move-beginning-of-line nil)
    (kill-line 1)
    (move-end-of-line nil)
    (when (< pos (point))
      (goto-char pos))))

(defun ml/insert-random-seed ()
  "Insert a random 64-bit integer at point (32-bit with prefix arg)."
  (interactive)
  (let ((seed (random (ash 1 (if current-prefix-arg 32 64)))))
    (insert (format "%d" seed))))

(provide 'mlutils)
;;; mlutils.el ends here
