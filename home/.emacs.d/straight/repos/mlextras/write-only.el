;;; write-only.el --- Hide everything except the current sentence you are writing

;;; Commentary:

;;; Code:

(defun write-only-sentence-range ()
  (bounds-of-thing-at-point 'sentence))

(defun write-only-make-background-overlay ()
  (let ((ol (make-overlay (point-min) (point-max))))
    (overlay-put ol 'priority (1- hl-line-overlay-priority))
    (overlay-put ol 'face 'write-only-dim-background)
    ol))

(defgroup write-only nil
  "Hide everything except the current sentence you are writing."
  :group 'convenience)

(defface write-only-dim-background
  '((t :inherit shadow :foreground "#2E3440"))
  "Default face for dimming/hiding everything that is not the current sentence."
  :group 'write-only)

(defface write-only-hl-sentence
  '((t :inherit default))
  "Default face for highlighting the current sentence."
  :group 'write-only)

(defvar-local write-only-background-overlay nil
  "The background overlay.")

(define-minor-mode write-only-mode
  "Hide everything except the current sentence you are writing."
  :init-value nil
  (if write-only-mode
      (progn
        (make-local-variable 'hl-line-range-function)
        (make-local-variable 'hl-line-face)
        (setq hl-line-range-function #'write-only-sentence-range
              hl-line-face 'write-only-hl-sentence
              write-only-background-overlay (write-only-make-background-overlay))
        (hl-line-mode +1))
    (hl-line-mode -1)
    (kill-local-variable 'hl-line-range-function)
    (kill-local-variable 'hl-line-face)
    (delete-overlay write-only-background-overlay)
    (setq write-only-background-overlay nil)))

(provide 'write-only)
;;; write-only.el ends here
