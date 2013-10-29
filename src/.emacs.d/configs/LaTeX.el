(require 'tex-site)
(require 'ac-math)

(defun ac-latex-mode-setup ()
  (setq ac-sources
        (append '(ac-source-math-unicode ac-source-math-latex ac-source-latex-commands)
                ac-sources)))
                
(add-to-list 'ac-modes 'latex-mode)
(add-hook 'LaTeX-mode-hook 'auto-complete-mode)
(add-hook 'LaTeX-mode-hook (lambda () (company-mode nil)))
(add-hook 'LaTeX-mode-hook 'ac-latex-mode-setup)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(setq TeX-electric-sub-and-superscript t)
(setq TeX-PDF-mode t) ; Produce PDFs
(setq TeX-save-query nil) ; Autosave files before compilation
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
