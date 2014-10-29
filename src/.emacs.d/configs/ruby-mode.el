(defun cqql/disable-ruby-lint-checker ()
  (cqql/after-load 'flycheck
    (let* ((checkers (flycheck-checker-next-checkers 'ruby-rubocop))
           (filtered (-filter (lambda (e) (not (eq 'ruby-rubylint (cdr e)))) checkers)))
      (put 'ruby-rubocop 'flycheck-next-checkers filtered))))

(cqql/after-load 'ruby-mode
  (setf ruby-insert-encoding-magic-comment nil)

  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
  (add-hook 'ruby-mode-hook 'yard-mode)
  (add-hook 'ruby-mode-hook 'eldoc-mode)
  (add-hook 'ruby-mode-hook 'subword-mode)
  (add-hook 'ruby-mode-hook 'flycheck-mode)
  (add-hook 'ruby-mode-hook 'cqql/disable-ruby-lint-checker))

(cqql/add-auto-mode 'ruby-mode
                    "Rakefile\\'"
                    "Capfile\\'"
                    "Vagrantfile\\'"
                    "Berksfile\\'"
                    ".gemspec\\'"
                    ".json_builder\\'"
                    "Gemfile\\'")
