;; Workaround for smartparens overwriting `
(require 'smartparens-latex)

(require 'latex)
(require 'tex-site)

(add-hook 'LaTeX-mode-hook 'ax-latex-mode-setup)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)

(setq TeX-electric-sub-and-superscript t
      TeX-save-query nil
      TeX-view-program-selection '((output-pdf "Okular")))

(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
