(require 'yasnippet)

(setq yas-fallback-behavior 'call-other-command)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd ";") 'yas-expand)
; Don't append newlines to snippet files
(add-hook 'snippet-mode (lambda () (setq require-final-newline nil)))
(yas/load-directory "~/.emacs.d/snippets")
(yas-global-mode t)
