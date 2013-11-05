(require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)
(show-smartparens-global-mode t)

(defconfig smartparens
  :evil-keys ((insert "C-M-f" 'sp-next-sexp)
              (insert "C-M-S-f" 'sp-forward-sexp)
              (insert "C-M-b" 'sp-backward-sexp)
              (insert "C-M-S-b" 'sp-previous-sexp)
              (insert "C-M-n" 'sp-down-sexp)
              (insert "C-M-S-n" 'sp-backward-down-sexp)
              (insert "C-M-p" 'sp-up-sexp)
              (insert "C-M-S-p" 'sp-backward-up-sexp)
              (insert "C-M-a" 'sp-beginning-of-sexp)
              (insert "C-M-e" 'sp-end-of-sexp)))
