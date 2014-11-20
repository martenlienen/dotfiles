(defun cqql/dired-jump-to-first-file ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun cqql/dired-jump-to-last-file ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  [remap beginning-of-buffer] 'cqql/dired-jump-to-first-file)

(define-key dired-mode-map
  [remap end-of-buffer] 'cqql/dired-jump-to-last-file)
