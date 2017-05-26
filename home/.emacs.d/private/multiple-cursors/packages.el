;;; packages.el --- multiple-cursors layer packages file for Spacemacs.

;;; Commentary:

;;; Code:

(defconst multiple-cursors-packages
  '(multiple-cursors))

(defun multiple-cursors/init-multiple-cursors ()
  (use-package multiple-cursors
	       :bind ("M-n" . cqql-multiple-cursors-hydra/body)))

(defun multiple-cursors/post-init-multiple-cursors ()
  (defhydra cqql-multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil)))

;;; packages.el ends here
