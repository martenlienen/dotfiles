(defun rst-length-of-line ()
  "Return length of the current line."
  (save-excursion
    (- (progn (end-of-line) (point))
       (progn (beginning-of-line) (point)))))

(defun rst-underline ()
  "Repeat the character at point until it stretches the length of
  the previous line."
  (interactive)
  (let* ((char (preceding-char))
         (prev-length (save-excursion
                        (forward-line -1)
                        (rst-length-of-line)))
         (curr-length (save-excursion
                        (rst-length-of-line)))
         (repeat-length (max 0 (- prev-length curr-length))))
    (insert (s-repeat repeat-length (char-to-string char)))))
