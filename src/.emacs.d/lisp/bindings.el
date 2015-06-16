(defhydra cqql/org-hydra (:exit t)
  "Quick-Access to Org-Files"
  ("n"
   (lambda () (interactive) (find-file (f-join org-directory "inbox.org")))
   "Notes")
  ("i"
   (lambda () (interactive) (find-file (f-join org-directory "ideas.org")))
   "Ideas")
  ("u"
   (lambda () (interactive) (find-file (f-join org-directory "uni.org")))
   "University")
  ("v"
   (lambda () (interactive) (find-file (f-join org-directory "vitakid.org")))
   "vitakid")
  ("p"
   (lambda () (interactive) (find-file (f-join org-directory "passwords.org")))
   "Passwords"))

(defhydra cqql/multiple-cursors-hydra (:hint nil)
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
  ("q" nil))

(cqql/define-global-keys
  ("C-a" 'cqql/go-to-beginning-of-line-dwim)
  ("M-D" 'cqql/duplicate-text)
  ("M-n" 'cqql/multiple-cursors-hydra/body)
  ("C-S-k" 'cqql/kill-line)
  ("C-o" 'cqql/open-line)
  ("C-S-o" 'cqql/open-line-above)
  ("<f6>" 'cqql/org-hydra/body))

(provide 'bindings)
