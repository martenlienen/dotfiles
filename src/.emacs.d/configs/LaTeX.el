;; Workaround for smartparens overwriting `
(require 'smartparens-latex)

(require 'latex)
(require 'tex-site)
(require 'preview)

(add-hook 'LaTeX-mode-hook 'ax-latex-mode-setup)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq TeX-electric-sub-and-superscript t
                                   TeX-save-query nil
                                   TeX-view-program-selection '((output-pdf "Okular"))
                                   ;; Otherwise minted can't find pygments
                                   TeX-command-extra-options "-shell-escape")))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
