;;; packages.el --- cqql layer packages file for Spacemacs.

;;; Commentary:

;;; Code:

(defconst cqql-packages
  '((cqql :location local)))

(defun cqql/init-cqql ()
  (use-package cqql
    :bind (:map evil-hybrid-state-map
                ("C-a" . cqql-go-to-beginning-of-line-dwim)
                ("M-D" . cqql-duplicate-text)
                ("C-S-k" . cqql-kill-line)
                ("C-o" . cqql-open-line)
                ("C-S-o" . cqql-open-line-above)
                ("C-S-p" . cqql-move-text-up)
                ("C-S-n" . cqql-move-text-down)))

  (use-package dired
    :config
    (define-key dired-mode-map
      [remap beginning-of-buffer] #'cqql-dired-jump-to-first-file)

    (define-key dired-mode-map
      [remap end-of-buffer] #'cqql-dired-jump-to-last-file))

  (use-package python
    :config
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "sl" 'cqql-python-shell-send-line
      "ss" 'cqql-python-shell-send-region-dwim)))

;;; packages.el ends here
