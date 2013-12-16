(require 'yasnippet)

(setq yas-fallback-behavior 'call-other-command)

;; Don't append newlines to snippet files
(add-hook 'snippet-mode (lambda () (setq require-final-newline nil)))

;; Don't remove whitespace in yasnippets
(add-to-list 'cqql/no-trimming-modes 'snippet-mode)

(yas/load-directory "~/.emacs.d/snippets")
(yas-global-mode t)
