(defun cqql/go-to-beginning-of-line-dwim ()
  (interactive)
  "Toggle point between beginning of line and first non-whitespace character"
  (let ((prev-pos (point)))
    (back-to-indentation)
    (when (= prev-pos (point))
      (move-beginning-of-line nil))))

(defun cqql/duplicate-line (times)
  (interactive "p")
  "Duplicate the current line TIMES times"
  (let* ((pos (point))
         (line-start (save-excursion
                       (beginning-of-line)
                       (point)))
         (line-end (save-excursion
                     (end-of-line)
                     (point)))
         (line (buffer-substring line-start line-end))
         (new-pos (+ pos (* times (length line)) 1)))
    (dotimes (i times)
      (end-of-line)
      (insert "\n")
      (insert line))
    (setf (point) new-pos)))

(defun cqql/open-line ()
  (interactive)
  "Create a new line below and put point into it"
  (move-end-of-line nil)
  (newline-and-indent))

(defun cqql/open-line-above ()
  (interactive)
  "Create a new line above point and move point into it"
  (move-beginning-of-line nil)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))

(defun cqql/kill-line ()
  (interactive)
  "Kill the current line"
  (move-beginning-of-line nil)
  (kill-line 1))

(defvar cqql/no-trimming-modes '()
  "A list of modes, that should not be whitespace-trimmed")

(defun cqql/trim-whitespace ()
  (when (not (-contains? cqql/no-trimming-modes major-mode))
    (delete-trailing-whitespace)))

(defmacro cqql/after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(defmacro cqql/define-keys (keymap &rest bindings)
  `(progn
     ,@(-map
        (lambda (binding) `(define-key ,keymap (kbd ,(car binding)) ,(cadr binding)))
        bindings)))

(defmacro cqql/define-global-keys (&rest bindings)
  `(progn
     ,@(-map
        (lambda (binding) `(global-set-key (kbd ,(car binding)) ,(cadr binding)))
        bindings)))

(provide 'cqql-utils)
