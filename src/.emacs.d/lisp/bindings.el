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

(cqql/define-global-keys
 ("M-y" 'helm-show-kill-ring)
 ("C-x b" 'helm-mini)
 ("C-x C-b" 'ibuffer)
 ("M-/" 'hippie-expand)
 ("C-M-SPC" 'company-complete)
 ("C-a" 'cqql/go-to-beginning-of-line-dwim)
 ("M-D" 'cqql/duplicate-text)
 ("C->" 'mc/mark-next-like-this)
 ("C-M->" 'mc/skip-to-next-like-this)
 ("C-<" 'mc/unmark-next-like-this)
 ("M-n" 'mc/mark-all-like-this)
 ("C-M-n" 'mc/edit-lines)
 ("M-m" 'er/expand-region)
 ("M-M" 'er/contract-region)
 ("C-S-p" 'move-text-up)
 ("C-S-n" 'move-text-down)
 ("C-S-k" 'cqql/kill-line)
 ("C-o" 'cqql/open-line)
 ("C-S-o" 'cqql/open-line-above)
 ("C-x C-a" 'ag-project-regexp)
 ("C-x M-a" 'ag-regexp)
 ("C-x C-f" 'helm-projectile)
 ("C-x f" 'helm-find-files)
 ("M-s" 'ace-jump-word-mode)
 ("M-S" 'ace-jump-char-mode)
 ("M-z" 'ace-jump-zap-to-char)
 ("M-Z" 'ace-jump-zap-up-to-char)
 ("M-i" 'ace-window)
 ("M-o" 'helm-swoop)
 ("M-x" 'helm-M-x)
 ("C-z" popwin:keymap)
 ("C-c a" 'org-agenda)
 ("C-c c" 'org-capture)
 ("C-h C-m" 'discover-my-major)
 ("M-3" 'vr/replace)
 ("M-#" 'vr/query-replace)
 ("<f2>" 'magit-status)
 ("<f6>" 'cqql/org-hydra/body))

(cqql/define-keys emacs-lisp-mode-map
                  ("C-h C-f" 'find-function))

(cqql/define-keys smartparens-mode-map
                  ("C-M-f" 'sp-forward-sexp)
                  ("C-M-S-f" 'sp-next-sexp)
                  ("C-M-b" 'sp-backward-sexp)
                  ("C-M-S-b" 'sp-previous-sexp)
                  ("C-M-n" 'sp-down-sexp)
                  ("C-M-S-n" 'sp-backward-down-sexp)
                  ("C-M-p" 'sp-up-sexp)
                  ("C-M-S-p" 'sp-backward-up-sexp)
                  ("C-M-a" 'sp-beginning-of-sexp)
                  ("C-M-e" 'sp-end-of-sexp)
                  ("C-M-k" 'sp-kill-sexp)
                  ("C-M-S-k" 'sp-backward-kill-sexp)
                  ("C-M-w" 'sp-copy-sexp)
                  ("C-M-t" 'sp-transpose-sexp)
                  ("C-M-h" 'sp-backward-slurp-sexp)
                  ("C-M-S-h" 'sp-backward-barf-sexp)
                  ("C-M-l" 'sp-forward-slurp-sexp)
                  ("C-M-S-l" 'sp-forward-barf-sexp)
                  ("C-M-j" 'sp-splice-sexp)
                  ("C-M-S-j" 'sp-raise-sexp))

(cqql/after-load 'ruby-mode
  (cqql/define-keys ruby-mode-map
                    ("C-c f" 'rspec-verify-single)
                    ("C-c r r" 'rspec-rerun)
                    ("C-c r f" 'rspec-verify)
                    ("C-c r g" 'rspec-verify-all)))

(cqql/define-keys yas-minor-mode-map
                  ("<tab>" nil)
                  ("TAB" nil)
                  (";" 'yas-expand))

(cqql/after-load 'latex
  (cqql/define-keys LaTeX-mode-map
                    ("C-c u" (lambda () (interactive) (insert "ü")))
                    ("C-c U" (lambda () (interactive) (insert "Ü")))
                    ("C-c a" (lambda () (interactive) (insert "ä")))
                    ("C-c A" (lambda () (interactive) (insert "Ä")))
                    ("C-c o" (lambda () (interactive) (insert "ö")))
                    ("C-c O" (lambda () (interactive) (insert "Ö")))
                    ("C-c s" (lambda () (interactive) (insert "ß")))
                    ("<f6>" 'preview-section)
                    ("S-<f6>" 'preview-clearout-section)
                    ("<f7>" 'preview-buffer)
                    ("S-<f7>" 'preview-clearout-buffer)))

(cqql/after-load 'helm-swoop
  (cqql/define-keys helm-swoop-map
                    ("M-o" 'helm-multi-swoop-all-from-helm-swoop)))

(provide 'bindings)
