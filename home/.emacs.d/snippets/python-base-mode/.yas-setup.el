;;; yas-setup --- Setup for python snippets

;;; Commentary:

;;; Code:

(eval-when-compile
  (package-initialize))

(require 'python)
(require 's)

(defun isnip-python-maybe-prepend-comma (text)
  "Prepend ', ' if TEXT is not empty"
  (if (or (string= text "") (s-starts-with? "," text))
      (if (string-match "^, *$" text)
          ""
        text)
    (concat ", " text)))

(defun isnip-python-maybe-prepend-comma-in-class (text)
  "Maybe prepend a comma, if the def is inside a class."
  (if (isnip-python-inside-class?)
      (isnip-python-maybe-prepend-comma text)
    text))

(defun isnip-python-args-to-assignments (text)
  "Convert a string of arguments to a string of self assignments."
  (if (string= text "")
      ""
    (cl-flet ((strip-decorations (spec) (string-trim-left (nth 0 (split-string spec ":\\|=")) "\\**"))
              (make-assignment (arg) (format "self.%s = %s" arg arg))
              (indent (line) (concat (s-repeat (current-indentation) " ")
                                     line)))
      (let* ((specs (split-string text ", *" t))
             (args (mapcar #'strip-decorations specs))
             (assignments (mapcar #'make-assignment args))
             (indented (cons (car assignments)
                             (mapcar #'indent (cdr assignments)))))
        (s-join "\n" indented)))))

(defun isnip-python-inside-class? ()
  "Return t if point is directly inside a class."
  (save-excursion
    (let ((indentation (current-indentation)))
      (while (and (> (current-indentation) 0) (>= (current-indentation) indentation))
        (python-nav-beginning-of-defun))
      (looking-at "class"))))

(defun isnip-python-self-if-in-class ()
  "Return 'self' if point is inside a class."
  (if (isnip-python-inside-class?) "self" ""))

(defun isnip-true-beginning-of-line-p ()
  "Is point at the actual beginning of a line (disregarding indentation)?"
  (save-excursion
    ;; Skip over the key of the triggering template
    (backward-word)
    (or (= (point) (point-min)) (char-equal (char-before) ?\n))))
