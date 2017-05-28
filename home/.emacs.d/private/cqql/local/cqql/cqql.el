;;; cqql --- My custom functions

;;; Commentary:

;;; Code:

;;;###autoload
(defun cqql-go-to-beginning-of-line-dwim ()
  "Toggle point between beginning of line and first non-whitespace character."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (when (= prev-pos (point))
      (move-beginning-of-line nil))))

;;;###autoload
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

;;;###autoload
(defun cqql-move-text-up (lines)
  "Move the current line or region LINES lines up."
  (interactive "p")
  (cqql-move-text (- lines)))

;;;###autoload
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

;;;###autoload
(defun cqql-open-line ()
  "Create a new line below and put point into it."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;;;###autoload
(defun cqql-open-line-above ()
  "Create a new line above point and move point into it."
  (interactive)
  (move-beginning-of-line nil)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

;;;###autoload
(defun cqql-kill-line ()
  "Kill the current line."
  (interactive)
  (let ((pos (point)))
    (move-beginning-of-line nil)
    (kill-line 1)
    (move-end-of-line nil)
    (when (< pos (point))
      (setf (point) pos))))

;;;###autoload
(defun cqql-c-append-semicolon ()
  "Insert semicolon at the end of the line."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (insert ";")))

;;;###autoload
(defun cqql-latex-append-line-break ()
  "Insert the \\\\ macro at the end of the line."
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (insert "\\\\")))

;;;###autoload
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

;;;###autoload
(defun cqql-dired-jump-to-first-file ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

;;;###autoload
(defun cqql-dired-jump-to-last-file ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

;;;###autoload
(defun cqql-apply-command-to-buffer (command)
  "Apply shell command COMMAND to the current buffer."
  (interactive "sCommand:")
  (let ((p (point)))
    (shell-command-on-region (point-min) (point-max) command t t)
    (setf (point) p)))

(defmacro with-pyenv (name &rest body)
  "Execute BODY with pyenv NAME activated."
  (declare (indent defun))
  `(let ((current (pyenv-mode-version)))
     (unwind-protect
         (progn
           (pyenv-mode-set ,name)
           ,@body)
       (pyenv-mode-set current))))

;;;###autoload
(defun cqql-python-shell-send-line ()
  "Send the current line to inferior python process disregarding indentation."
  (interactive)
  (let ((start (save-excursion
                 (back-to-indentation)
                 (point)))
        (end (save-excursion
               (end-of-line)
               (point))))
    (python-shell-send-string (buffer-substring start end))))

(require 'cl-lib)

(defvar cqql-python-last-command nil
  "Stores the last sent region for resending.")

;;;###autoload
(defun cqql-python-shell-send-region ()
  "Send the current region to inferior python process stripping indentation."
  (interactive)
  (let* ((start (save-excursion
                  (goto-char (region-beginning))
                  (beginning-of-line)
                  (point)))
         (end (save-excursion
                (goto-char (region-end))
                (end-of-line)
                (point)))
         (region (buffer-substring start end))
         (command))
    ;; Strip indentation
    (with-temp-buffer
      (insert region)

      ;; Clear leading empty lines
      (goto-char (point-min))
      (while (char-equal (following-char) ?\n)
        (delete-char 1))

      ;; Remove indentation from all non-empty lines
      (let ((indent (save-excursion
                      (back-to-indentation)
                      (- (point) (point-min)))))
        (cl-loop until (eobp)
                 do
                 ;; Make sure that we do not delete empty lines or lines with
                 ;; only spaces but fewer than indent
                 (cl-loop repeat indent
                          while (char-equal (following-char) ?\s)
                          do (delete-char 1))
                 (forward-line 1)))
      (setq command (buffer-string)))
    (setq cqql-python-last-command command)
    (python-shell-send-string command)))

;;;###autoload
(defun cqql-python-shell-resend-last-command ()
  "Resend the last command to the inferior python process."
  (interactive)
  (when cqql-python-last-command
    (python-shell-send-string cqql-python-last-command)))

;;;###autoload
(defun cqql-python-shell-send-region-dwim ()
  "Send active region or resend last region."
  (interactive)
  (if (use-region-p)
      (cqql-python-shell-send-region)
    (cqql-python-shell-resend-last-command)))

(provide 'cqql)
;;; cqql.el ends here
