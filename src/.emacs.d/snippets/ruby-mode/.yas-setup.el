;;; yas-setup --- Setup for ruby snippets

;;; Commentary:

;;; Code:

(require 's)
(require 'f)

(defun isnip-ruby-guess-class-name ()
  "Guess a class name from the current file name."
  (let ((file-name (buffer-file-name)))
    (if file-name
        (s-upper-camel-case (f-filename (f-no-ext file-name)))
      "")))

(defun isnip-ruby-args-to-assignments (text)
  "Convert a string of arguments to a string of self assignments."
  (if (string= text "")
      ""
    (cl-flet ((make-assignment (arg) (format "self.%s = %s" arg arg))
              (indent (line) (concat (s-repeat (current-indentation) " ")
                                     line)))
      (let* ((args (split-string text ", *" t))
             (assignments (mapcar #'make-assignment args))
             (indented (cons (car assignments)
                             (mapcar #'indent (cdr assignments)))))
        (s-join "\n" indented)))))
